// Copyright 2017 Andrew Pennebaker

#include "fewer.h"

#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#if defined(_MSC_VER)
    #include <direct.h>
    #include <io.h>
#else
    #include <unistd.h>
#endif

#if defined(_MSC_VER) || defined(__minix)
    #include <stdarg.h>
#endif

#if defined(_MSC_VER)
    int fchdir(int fd) {
        DWORD result;
        unsigned int base_path_size = _XOPEN_PATH_MAX;
        char base_path[base_path_size];

        result = GetFinalPathNameByHandleA(
            (HANDLE) fd,
            base_path,
            base_path_size - 1,
            FILE_NAME_NORMALIZED
        );

        if (result > base_path_size) {
            return -1;
        }

        if (result == 0) {
            return -1;
        }

        errno = 0;
        fd = _chdir(base_path);
        return fd;
    }
#endif

#if defined(_MSC_VER) || defined(__minix)
    int openat(int fd, const char *path, int flags, ...) {
        mode_t mode = 0;

        if (flags & O_CREAT) {
            va_list arg;
            va_start(arg, flags);
            mode = va_arg(arg, mode_t);
            va_end(arg);
        }

        errno = 0;
        if (fchdir(fd) != 0) {
            return -1;
        }

        errno = 0;

        #if defined(_MSC_VER)
            if (_sopen_s(&fd, path, flags, _SH_DENYNO, mode) != 0) {
                return -1;
            }
        #else
            fd = open(path, flags, mode);
        #endif

        return fd;
    }
#endif

static const char *PROMPT = "> ";

int show_commands(FILE *console) {
    fprintf(console, "l <path>\tLoad file\n");
    fprintf(console, "n\t\tShow next byte\n");
    fprintf(console, "r <hex pair>\tRender an input byte\n");
    fprintf(console, "q\t\tQuit\n");

    #if defined(__CloudABI__)
        errno = 0;
        return fflush(console);
    #else
        return 0;
    #endif
}

void render_boi(FILE *console, unsigned int b, /*@out@*/ char *s, size_t s_len) {
    int write_count = snprintf(s, s_len, "%02x", b);

    if (write_count < 0) {
        fprintf(console, "Encoding error\n");
    } else if ((size_t) write_count > s_len - 1) {
        fprintf(console, "Buffer requires %d character allocation to render value\n", write_count);
    }
}

short parse_boi(char *s) {
    unsigned long int n = strtoul(s, NULL, 16);

    if (n == ULONG_MAX) {
        return -1;
    }

    if (n > UINT_MAX) {
        return -1;
    }

    return (short) n;
}

void chomp(char *s, size_t length) {
    if (length == 0) {
        return;
    }

    if (s[length - 1] == '\r') {
        s[length - 1] = '\0';
        return;
    }

    if (s[length - 1] == '\n') {
        s[length - 1] = '\0';
    }

    if (length > 1 && s[length - 2] == '\r') {
        s[length - 2] = '\0';
    }
}

bool validate_fewer_config(fewer_config *config) {
    if (config->console_err == NULL) {
        return false;
    }

    if (config->test) {
        return true;
    }

    if (config->root == -1) {
        fprintf(config->console_err, "root is unset\n");
        return false;
    }

    if (config->console_out == NULL) {
        fprintf(config->console_err, "console_out is unset\n");
        return false;
    }

    if (config->console_in == NULL) {
        fprintf(config->console_err, "console_in is unset\n");
        return false;
    }

    return true;
}

int unit_test(fewer_config *config) {
    size_t hex_buf_size = 3;
    char hex_buf[hex_buf_size];

    for (short d, c = 0; c < 0x100; c++) {
        render_boi(config->console_err, (unsigned int) c, &hex_buf[0], hex_buf_size);
        d = parse_boi(hex_buf);

        if (d == -1) {
            fprintf(config->console_err, "Error parsing hexadecimal sequence %s\n", hex_buf);
            return EXIT_FAILURE;
        }

        if (d != c) {
            fprintf(config->console_err, "Character %02x corrupted to %02x during hexadecimal translation\n", c, d);
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}

int repl(fewer_config *config) {
    int fd, c;
    size_t
        hex_buf_size = 3,
        command_size = 1,
        content_size = _XOPEN_PATH_MAX;
    size_t instruction_size = command_size + 1 + content_size;
    FILE *f = NULL;
    char
        hex_buf[hex_buf_size],
        instruction[instruction_size];
    char
        command = '\0',
        *content = NULL;

    if (!validate_fewer_config(config)) {
        return EXIT_FAILURE;
    }

    while (true) {
        fprintf(config->console_out, "%s", PROMPT);

        #if defined(__CloudABI__)
            errno = 0;
            if (fflush(config->console_out) == EOF) {
                perror(NULL);
                return EXIT_FAILURE;
            }
        #endif

        if (fgets(instruction, (int) instruction_size, config->console_in) == NULL) {
            if (ferror(config->console_in) != 0) {
                return EXIT_FAILURE;
            }

            return EXIT_SUCCESS;
        }

        chomp(instruction, strlen(instruction));

        if (strlen(instruction) == 0) {
            errno = 0;
            if (show_commands(config->console_err) == EOF) {
                perror(NULL);
                return EXIT_FAILURE;
            }

            continue;
        }

        command = instruction[0];

        switch (command) {
            case 'l':
                content = strchr(instruction, ' ');

                if (content == NULL || strlen(content) < 2) {
                    if (show_commands(config->console_err) == EOF) {
                        return EXIT_FAILURE;
                    }

                    break;
                }

                content++;

                if (f != NULL) {
                    if (fclose(f) == EOF) {
                        fprintf(config->console_err, "Error closing file\n");
                        return EXIT_FAILURE;
                    }
                }

                errno = 0;
                fd = openat(config->root, content, O_RDONLY);

                if (fd == -1) {
                    perror(NULL);

                    #if defined(__CloudABI__)
                        errno = 0;
                        if (fflush(config->console_err) == EOF) {
                            perror(NULL);
                            return EXIT_FAILURE;
                        }
                    #endif

                    break;
                }

                errno = 0;

                #if defined(_MSC_VER)
                    f = _fdopen(fd, "rb");
                #else
                    f = fdopen(fd, "rb");
                #endif

                if (f == NULL) {
                    perror(NULL);

                    #if defined(__CloudABI__)
                        errno = 0;
                        if (fflush(config->console_err) == EOF) {
                            perror(NULL);
                            return EXIT_FAILURE;
                        }
                    #endif
                }

                break;
            case 'n':
                if (f == NULL) {
                    fprintf(config->console_err, "No file loaded\n");

                    #if defined(__CloudABI__)
                        errno = 0;
                        if (fflush(config->console_err) == EOF) {
                            perror(NULL);
                            return EXIT_FAILURE;
                        }
                    #endif

                    break;
                }

                errno = 0;
                c = fgetc(f);

                if (c == EOF) {
                    if (ferror(f) != 0) {
                        fprintf(config->console_err, "Error reading character from file\n");
                        return EXIT_FAILURE;
                    }

                    return EXIT_SUCCESS;
                }

                render_boi(config->console_err, (unsigned int) c, &hex_buf[0], hex_buf_size);
                fprintf(config->console_out, "%s\n", hex_buf);

                #if defined(__CloudABI__)
                    errno = 0;
                    if (fflush(config->console_out) == EOF) {
                        perror(NULL);
                        return EXIT_FAILURE;
                    }
                #endif

                break;
            case 'r':
                content = strchr(instruction, ' ');

                if (content == NULL || strlen(content) < 2) {
                    if (show_commands(config->console_err) == EOF) {
                        return EXIT_FAILURE;
                    }

                    break;
                }

                content++;

                if (strlen(content) > hex_buf_size - 1) {
                    if (show_commands(config->console_err) == EOF) {
                        return EXIT_FAILURE;
                    }

                    break;
                }

                #if defined(_MSC_VER)
                    strncpy_s(hex_buf, hex_buf_size, content, strlen(content));
                #else
                    strncpy(hex_buf, content, hex_buf_size);
                #endif

                c = (int) parse_boi(hex_buf);

                if (c == -1) {
                    fprintf(config->console_err, "Error parsing hexadecimal %s\n", hex_buf);

                    #if defined(__CloudABI__)
                        errno = 0;
                        if (fflush(config->console_err) == EOF) {
                            perror(NULL);
                            return EXIT_FAILURE;
                        }
                    #endif

                    break;
                }

                fprintf(config->console_out, "%c\n", (unsigned int) c);

                #if defined(__CloudABI__)
                    errno = 0;
                    if (fflush(config->console_out) == EOF) {
                        perror(NULL);
                        return EXIT_FAILURE;
                    }
                #endif

                break;
            case 'q':
                if (f != NULL && fclose(f) == EOF) {
                    fprintf(config->console_err, "Error closing file\n");
                    return EXIT_FAILURE;
                }

                return EXIT_SUCCESS;
            default:
                if (show_commands(config->console_err) == EOF) {
                    return EXIT_FAILURE;
                }
        }
    }
}

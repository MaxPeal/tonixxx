// Copyright 2017 Andrew Pennebaker

#include "fewer.h"

#include <assert.h>
#include <fcntl.h>
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

#if defined(__minix) || defined(__sun) || defined(__HAIKU__)
    #include <stdarg.h>
#endif

#if defined(__minix)
    int openat(int fd, const char *path, int flags, ...) {
        mode_t mode = 0;
        int fd2;

        if (flags & O_CREAT) {
            va_list arg;
            va_start(arg, flags);
            mode = va_arg(arg, mode_t);
            va_end(arg);
        }

        // Path is simple, absolute
        if (fd == AT_FDCWD || path[0] == '/' || path[0] == '\\') {
            return open(path, flags, mode);
        }

        fd2 = open(".", O_SEARCH | O_CLOEXEC);

        // Detect collision with cwd
        if (fd >= 0 && fd == fd2) {
            return -1;
        }

        fd2 = fchdir(fd);

        if (!fd2) {
            fd2 = open(path, flags, mode);
        }

        return fd2;
    }
#elif defined(__sun) || defined(__HAIKU__)
    int dprintf(int fd, const char *restrict format, ...) {
        va_list ap;
        FILE *f = fdopen(fd, "w");

        if (!f) {
            return -1;
        }

        va_start(ap, format);
        int result = fprintf(f, format, ap);
        va_end(ap);

        return result;
    }
#endif

static const char *PROMPT = "> ";

void show_commands(int fd) {
    dprintf(fd, "l <path>\tLoad file\n");
    dprintf(fd, "n\t\tShow next byte\n");
    dprintf(fd, "r\t\tRender an input byte\n");
    dprintf(fd, "q\t\tQuit\n");
}

// Format a byte as a hexadecimal string
void render_boi(char b, /*@out@*/ char *s) {
    snprintf(s, 3, "%02x", b);
}

// Parse a hexadecimal string to a byte
unsigned char parse_boi(char *s) {
    return (unsigned char) strtol(s, NULL, 16);
}

void chomp(char *s) {
    int len = strlen(s);

    if (len == 0) {
        return;
    }

    if (s[len - 1] == '\r') {
        s[len - 1] = '\0';
        return;
    } else if (s[len - 1] == '\n') {
        s[len - 1] = '\0';

        if (len > 1 && s[len - 2] == '\r') {
            s[len - 2] = '\0';
        }
    }
}

fewer_config * new_fewer_config(size_t instruction_size) {
    fewer_config *config = malloc(sizeof(fewer_config));
    config->console_err = -1;
    config->console_out = -1;
    config->console_in = -1;
    config->root = -1;
    config->test = false;

    return config;
}

void destroy_fewer_config(fewer_config *config) {
    free(config);
}

void validate_fewer_config(fewer_config *config) {
    assert(config->root != -1 || config->test);
}

int repl(fewer_config *config) {
    int fd;
    unsigned char
        c,
        d;
    size_t
        read_count,
        command_size = 1,
        content_size = _XOPEN_PATH_MAX,
        instruction_size = command_size + 1 + content_size;
    FILE
        *stdin_f = NULL,
        *f = NULL;
    char
        command,
        *content = NULL,
        *char_buf = malloc(sizeof(char)),
        *hex_buf = malloc(3 * sizeof(char)),
        *instruction = calloc(instruction_size, sizeof(char));

    validate_fewer_config(config);

    if (config->test) {
        for (int i = 0; i <= CHAR_MAX; i++) {
            c = (char) i;

            render_boi(c, hex_buf);
            d = parse_boi(hex_buf);

            if (d != c) {
                dprintf(config->console_err, "Character %02x corrupted to %02x during hexadecimal translation\n", c, d);

                free(instruction);
                free(hex_buf);
                free(char_buf);
                return EXIT_FAILURE;
            }
        }

        free(instruction);
        free(hex_buf);
        free(char_buf);
        return EXIT_SUCCESS;
    }

    #if defined(_MSC_VER)
        stdin_f = _fdopen(config->console_in, "r");
    #else
        stdin_f = fdopen(config->console_in, "r");
    #endif

    if (!stdin_f) {
        dprintf(config->console_err, "Error opening stdin\n");

        free(instruction);
        free(hex_buf);
        free(char_buf);
        return EXIT_FAILURE;
    }

    while (true) {
        if (feof(stdin_f)) {
            dprintf(config->console_out, "\n");

            free(instruction);
            free(hex_buf);
            free(char_buf);

            if (f && fclose(f) == EOF) {
                dprintf(config->console_err, "Error closing file\n");
                return EXIT_FAILURE;
            }

            return EXIT_SUCCESS;
        }

        dprintf(config->console_out, "%s", PROMPT);

        getline(&instruction, &instruction_size, stdin_f);

        if (!instruction) {
            return EXIT_FAILURE;
        }

        chomp(instruction);

        if (strlen(instruction) == 0) {
            show_commands(config->console_err);
            continue;
        }

        command = instruction[0];

        switch(command) {
            case 'l':
                content = strchr(instruction, ' ');

                if (!content || strlen(content) < 2) {
                    show_commands(config->console_err);
                    continue;
                }

                content++;

                if (f) {
                    if (fclose(f) == EOF) {
                        dprintf(config->console_err, "Error closing file\n");

                        free(instruction);
                        free(hex_buf);
                        free(char_buf);
                        return EXIT_FAILURE;
                    }
                }

                fd = openat(config->root, content, O_RDONLY);

                if (fd == -1) {
                    dprintf(config->console_err, "Error opening file description for %s\n", content);
                    continue;
                }

                #if defined(_MSC_VER)
                    f = _fdopen(fd, "rb");
                #else
                    f = fdopen(fd, "rb");
                #endif

                if (!f) {
                    dprintf(config->console_err, "Error opening file %s\n", content);
                }

                break;
            case 'n':
                if (!f) {
                    dprintf(config->console_err, "No file loaded\n");
                    continue;
                }

                read_count = fread(char_buf, 1, 1, f);

                if (read_count != 1) {
                    dprintf(config->console_err, "Error reading byte\n");

                    if (fclose(stdin_f) == EOF) {
                        dprintf(config->console_err, "Error closing stdin\n");
                    }

                    if (fclose(f) == EOF) {
                        dprintf(config->console_err, "Error closing file\n");
                    }

                    free(instruction);
                    free(hex_buf);
                    free(char_buf);
                    return EXIT_FAILURE;
                }

                render_boi(char_buf[0], hex_buf);
                dprintf(config->console_out, "%s\n", hex_buf);
                break;
            case 'r':
                #if defined(_MSC_VER)
                    read_count = fscanf_s(stdin_f, "%2s", hex_buf, hex_buf_size);
                #else
                    read_count = fscanf(stdin_f, "%2s", hex_buf);
                #endif

                if (read_count != 1) {
                    show_commands(config->console_err);
                    continue;
                }

                c = parse_boi(hex_buf);
                dprintf(config->console_out, "%c\n", c);
                break;
            case 'q':
                free(instruction);
                free(hex_buf);
                free(char_buf);

                if (f && fclose(f) == EOF) {
                    dprintf(config->console_err, "Error closing file\n");
                    return EXIT_FAILURE;
                }

                if (fclose(stdin_f)) {
                    dprintf(config->console_err, "Error closing stdin\n");
                    return EXIT_FAILURE;
                }

                return EXIT_SUCCESS;
            default:
                show_commands(config->console_err);
        }
    }
}

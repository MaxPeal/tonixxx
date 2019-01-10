// Copyright 2017 Andrew Pennebaker

#include "fewer.h"

#include <assert.h>
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

    #if defined(_MSC_VER)
        int fchdir(int fd) {
            DWORD result;
            unsigned int base_path_size = _XOPEN_PATH_MAX;
            char *base_path = NULL;
            base_path = malloc(base_path_size * sizeof(char));
            assert(base_path);

            result = GetFinalPathNameByHandleA(
                (HANDLE) fd,
                base_path,
                base_path_size - 1,
                FILE_NAME_NORMALIZED
            );

            if (result > base_path_size) {
                free(base_path);
                return -1;
            }

            if (result == 0) {
                free(base_path);
                return -1;
            }

            fd = _chdir(base_path);
            free(base_path);
            return fd;
        }
    #endif

    int openat(int fd, const char *path, int flags, ...) {
        mode_t mode = 0;

        if (flags & O_CREAT) {
            va_list arg;
            va_start(arg, flags);
            mode = va_arg(arg, mode_t);
            va_end(arg);
        }

        if (fchdir(fd) != 0) {
            return -1;
        }

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

void show_commands(FILE *console) {
    fprintf(console, "l <path>\tLoad file\n");
    fprintf(console, "n\t\tShow next byte\n");
    fprintf(console, "r <hex pair>\tRender an input byte\n");
    fprintf(console, "q\t\tQuit\n");

    #if defined(__CloudABI__)
        fflush(console);
    #endif
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

fewer_config * new_fewer_config() {
    fewer_config *config = malloc(sizeof(fewer_config));
    assert(config);
    config->console_err = NULL;
    config->console_out = NULL;
    config->console_in = NULL;
    config->root = -1;
    config->test = false;
    return config;
}

void destroy_fewer_config(fewer_config *config) {
    free(config);
}

bool validate_fewer_config(fewer_config *config) {
    return config->root != -1 || config->test;
}

int repl(fewer_config *config) {
    int root, fd;
    bool test, test_passing = true;
    unsigned char c, d;
    size_t
        command_size = 1,
        content_size = _XOPEN_PATH_MAX,
        instruction_size = command_size + 1 + content_size,
        hex_buf_size = 3;
    FILE
        *console_err,
        *console_out,
        *console_in,
        *f = NULL;
    char
        command,
        *content = NULL,
        *hex_buf = NULL,
        *char_buf = NULL,
        *instruction = NULL;

    if (!validate_fewer_config(config)) {
        return EXIT_FAILURE;
    }

    console_err = config->console_err;
    console_out = config->console_out;
    console_in = config->console_in;
    root = config->root;
    test = config->test;

    hex_buf = malloc(hex_buf_size * sizeof(char));
    assert(hex_buf);

    if (test) {
        for (int i = 0; i <= CHAR_MAX; i++) {
            c = (char) i;

            render_boi(c, hex_buf);
            d = parse_boi(hex_buf);

            if (d != c) {
                fprintf(console_err, "Character %02x corrupted to %02x during hexadecimal translation\n", c, d);
                test_passing = false;
                break;
            }
        }

        free(hex_buf);

        if (!test_passing) {
            return EXIT_FAILURE;
        }

        return EXIT_SUCCESS;
    }

    char_buf = malloc(sizeof(char));
    assert(char_buf);

    instruction = calloc(instruction_size, sizeof(char));
    assert(instruction);

    while (true) {
        if (feof(console_in)) {
            fprintf(console_out, "\n");
            free(instruction);
            free(char_buf);
            free(hex_buf);

            if (f && fclose(f) == EOF) {
                fprintf(console_err, "Error closing file\n");
                return EXIT_FAILURE;
            }

            return EXIT_SUCCESS;
        }

        fprintf(console_out, "%s", PROMPT);

        #if defined(__CloudABI__)
            fflush(console_out);
        #endif

        if (!fgets(instruction, instruction_size, console_in)) {
            free(instruction);
            free(char_buf);
            free(hex_buf);
            return EXIT_SUCCESS;
        }

        chomp(instruction);

        if (strlen(instruction) == 0) {
            show_commands(console_err);
            continue;
        }

        command = instruction[0];

        switch(command) {
            case 'l':
                content = strchr(instruction, ' ');

                if (!content || strlen(content) < 2) {
                    show_commands(console_err);
                    continue;
                }

                content++;

                if (f) {
                    if (fclose(f) == EOF) {
                        fprintf(console_err, "Error closing file\n");
                        free(instruction);
                        free(char_buf);
                        free(hex_buf);
                        return EXIT_FAILURE;
                    }
                }

                fd = openat(root, content, O_RDONLY);

                if (fd == -1) {
                    fprintf(console_err, "Error opening file description for %s\n", content);

                    #if defined(__CloudABI__)
                        fflush(console_err);
                    #endif

                    continue;
                }

                #if defined(_MSC_VER)
                    f = _fdopen(fd, "rb");
                #else
                    f = fdopen(fd, "rb");
                #endif

                if (!f) {
                    fprintf(console_err, "Error opening path %s\n", content);

                    #if defined(__CloudABI__)
                        fflush(console_err);
                    #endif
                }

                break;
            case 'n':
                if (!f) {
                    fprintf(console_err, "No file loaded\n");

                    #if defined(__CloudABI__)
                        fflush(console_err);
                    #endif
                    continue;
                }

                if (fread(char_buf, 1, 1, f) != 1) {
                    fprintf(console_err, "Error reading byte\n");
                    free(instruction);
                    free(char_buf);
                    free(hex_buf);
                    return EXIT_FAILURE;
                }

                render_boi(char_buf[0], hex_buf);
                fprintf(console_out, "%s\n", hex_buf);

                #if defined(__CloudABI__)
                    fflush(console_out);
                #endif

                break;
            case 'r':
                content = strchr(instruction, ' ');

                if (!content || strlen(content) < 2) {
                    show_commands(console_err);
                    continue;
                }

                content++;

                if (strlen(content) > hex_buf_size - 1) {
                    show_commands(console_err);
                    continue;
                }

                #if defined(_MSC_VER)
                    strncpy_s(hex_buf, hex_buf_size, content, strlen(content));
                #else
                    strncpy(hex_buf, content, hex_buf_size);
                #endif


                c = parse_boi(hex_buf);
                fprintf(console_out, "%c\n", c);

                #if defined(__CloudABI__)
                    fflush(console_out);
                #endif

                break;
            case 'q':
                free(instruction);
                free(char_buf);
                free(hex_buf);

                if (f && fclose(f) == EOF) {
                    fprintf(console_err, "Error closing file\n");
                    return EXIT_FAILURE;
                }

                return EXIT_SUCCESS;
            default:
                show_commands(console_err);
        }
    }
}

// Copyright 2017 Andrew Pennebaker

#define _GNU_SOURCE

#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <limits.h>

#ifdef _MSC_VER
    #include <direct.h>
    #include <io.h>
#else
    #include <unistd.h>
#endif

#ifndef _XOPEN_PATH_MAX
    #define _XOPEN_PATH_MAX _POSIX_PATH_MAX
#endif

#include "fewer.h"

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
        *stdin_file = NULL,
        *file = NULL;
    char
        command,
        *content = NULL,
        *char_buf = NULL,
        *instruction = NULL,
        *hex_buf = NULL;

    validate_fewer_config(config);

    hex_buf = malloc(3 * sizeof(char));

    if (config->test) {
        for (int i = 0; i <= CHAR_MAX; i++) {
            c = (char) i;

            render_boi(c, hex_buf);
            d = parse_boi(hex_buf);

            if (d != c) {
                dprintf(config->console_err, "Character %02x corrupted to %02x during hexadecimal translation\n", c, d);

                free(hex_buf);
                return EXIT_FAILURE;
            }
        }

        free(hex_buf);
        return EXIT_SUCCESS;
    }

    char_buf = malloc(sizeof(char)),
    instruction = malloc(instruction_size * sizeof(char)),

    #ifdef _MSC_VER
        stdin_file = _fdopen(config->console_in, "r");
    #else
        stdin_file = fdopen(config->console_in, "r");
    #endif

    if (!stdin_file) {
        dprintf(config->console_err, "Error opening stdin\n");

        free(instruction);
        free(char_buf);
        free(hex_buf);
        return EXIT_FAILURE;
    }

    while (true) {
        dprintf(config->console_out, "%s", PROMPT);

        getline(&instruction, &instruction_size, stdin_file);

        if (feof(stdin_file)) {
            dprintf(config->console_out, "\n");

            if (file) {
                (void) fclose(file);
            }

            free(instruction);
            free(char_buf);
            free(hex_buf);
            return EXIT_SUCCESS;
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

                if (file) {
                    (void) fclose(file);
                }

                fd = openat(config->root, content, O_RDONLY);

                if (fd == -1) {
                    dprintf(config->console_err, "Error opening file description for %s\n", content);
                    continue;
                }

                #ifdef _MSC_VER
                    file = _fdopen(fd, "r");
                #else
                    file = fdopen(fd, "r");
                #endif

                if (!file) {
                    dprintf(config->console_err, "Error opening file %s\n", content);
                }

                break;
            case 'n':
                if (!file) {
                    dprintf(config->console_err, "No file loaded\n");
                    continue;
                }

                read_count = fread(char_buf, 1, 1, file);

                if (read_count != 1) {
                    dprintf(config->console_err, "Error reading byte\n");

                    (void) fclose(stdin_file);
                    (void) fclose(file);
                    free(instruction);
                    free(char_buf);
                    free(hex_buf);
                    return EXIT_FAILURE;
                }

                render_boi(char_buf[0], hex_buf);
                dprintf(config->console_out, "%s\n", hex_buf);
                break;
            case 'r':
                #ifdef _MSC_VER
                    read_count = fscanf_s(stdin_file, "%2s", hex_buf, hex_buf_size);
                #else
                    read_count = fscanf(stdin_file, "%2s", hex_buf);
                #endif

                if (read_count != 2) {
                    show_commands(config->console_err);
                    continue;
                }

                c = parse_boi(hex_buf);
                dprintf(config->console_out, "%c\n", c);
                break;
            case 'q':
                (void) fclose(stdin_file);

                if (file) {
                    (void) fclose(file);
                }

                free(instruction);
                free(char_buf);
                free(hex_buf);
                return EXIT_SUCCESS;
            default:
                show_commands(config->console_err);
        }
    }
}

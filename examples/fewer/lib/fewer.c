// Copyright 2017 Andrew Pennebaker

#ifndef __CloudABI__
    #define _GNU_SOURCE
#endif

#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <limits.h>
#include "fewer.h"

static const char *PROMPT = "> ";

// Format a byte as a hexadecimal string
void render_boi(char b, /*@out@*/ char *s) {
    snprintf(s, 3, "%02x", b);
}

// Parse a hexadecimal string to a byte
unsigned char parse_boi(char *s) {
    return (unsigned char) strtol(s, NULL, 16);
}

void show_commands(int console_out) {
    dprintf(console_out, "l <path>\tLoad file\n");
    dprintf(console_out, "n\t\tShow next byte\n");
    dprintf(console_out, "r\t\tRender an input byte\n");
    dprintf(console_out, "q\t\tQuit\n");
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

        if (len - 1 > 0 && s[len - 2] == '\r') {
            s[len - 2] = '\0';
        }
    }
}

int repl(
    int console_out,
    int console_err,
    int console_in,
    int root,
    /*@out@*/ char *instruction,
    size_t instruction_size,
    /*@out@*/ char *buffer,
    /*@out@*/ char *hex_buf,
    size_t hex_buf_size
) {
    char *filename;
    int fd;
    size_t read_count;
    unsigned char c;
    FILE *console_in_file, *file;
    file = NULL;

    #ifdef _MSC_VER
        console_in_file = _fdopen(console_in, "r");
    #else
        console_in_file = fdopen(console_in, "r");
    #endif

    if (!console_in_file) {
        dprintf(console_err, "Error opening stdin\n");
        return EXIT_FAILURE;
    }

    while (true) {
        dprintf(console_out, "%s", PROMPT);
        getline(&instruction, &instruction_size, console_in_file);

        if (feof(console_in_file)) {
            dprintf(console_out, "\n");

            if (file) {
                (void) fclose(file);
            }

            return EXIT_SUCCESS;
        }

        chomp(instruction);

        switch (instruction[0]) {
            case 'l':
                if (strlen(instruction) < 3) {
                    show_commands(console_out);
                    continue;
                }

                if (file) {
                    (void) fclose(file);
                }

                filename = instruction + 2;

                fd = openat(root, filename, O_RDONLY);

                if (fd == -1) {
                    dprintf(console_err, "Error opening file description for %s\n", filename);
                    continue;
                }

                #ifdef _MSC_VER
                    file = _fdopen(fd, "r");
                #else
                    file = fdopen(fd, "r");
                #endif

                if (!file) {
                    dprintf(console_err, "Error opening file %s\n", filename);
                }

                break;
            case 'n':
                if (!file) {
                    dprintf(console_err, "No file loaded\n");
                    continue;
                }

                read_count = fread(buffer, 1, 1, file);

                if (read_count != 1) {
                    dprintf(console_err, "Error reading byte\n");
                    (void) fclose(console_in_file);
                    (void) fclose(file);
                    return EXIT_FAILURE;
                }

                render_boi(buffer[0], hex_buf);
                dprintf(console_out, "%s\n", hex_buf);
                break;
            case 'r':
                #ifdef _MSC_VER
                    read_count = fscanf_s(console_in_file, "%2s", hex_buf, hex_buf_size);
                #else
                    read_count = fscanf(console_in_file, "%2s", hex_buf);
                #endif

                if (read_count != 2) {
                    show_commands(console_out);
                    continue;
                }

                c = parse_boi(hex_buf);
                dprintf(console_out, "%c\n", c);
                break;
            case 'q':
                (void) fclose(console_in_file);

                if (file) {
                    (void) fclose(file);
                }

                return EXIT_SUCCESS;
            default:
                show_commands(console_out);
        }
    }
}

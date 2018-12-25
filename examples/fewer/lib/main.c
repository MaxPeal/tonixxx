// Copyright 2017 Andrew Pennebaker

#ifdef __CloudABI__
    #include <argdata.h>
    #include <program.h>
#else
    #define _GNU_SOURCE
    #include <limits.h>

    #ifdef _MSC_VER
        #include <direct.h>
        #include <io.h>
    #else
        #include <unistd.h>
    #endif
#endif

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "fewer.h"
#include "main.h"

int m(int console_out, int console_err, int console_in, int file, bool test) {
    char *buffer;
    char *instruction;
    size_t instruction_size = 1024;
    char *hex_buf;
    size_t hex_buf_size = 3;

    hex_buf = malloc(hex_buf_size * sizeof(char));
    assert(hex_buf != NULL);

    if (console_out == -1) {
        return EXIT_FAILURE;
    } else if (console_err == -1) {
        dprintf(console_out, "Error accessing stderr\n");
        return EXIT_FAILURE;
    } else if (!test && console_in == -1) {
        dprintf(console_err, "Error accessing stdin\n");
        return EXIT_FAILURE;
    } else if (!test && file == -1) {
        dprintf(console_err, "Error reading file\n");
        return EXIT_FAILURE;
    }

    if (test) {
        char c;

        for (int i = 0; i <= CHAR_MAX; i++) {
            c = (char) i;

            render_boi(c, hex_buf);
            unsigned char d = parse_boi(hex_buf);

            if (d != c) {
                dprintf(console_err, "Character %02x corrupted to %02x during hexadecimal translation\n", c, d);
                free(hex_buf);
                return EXIT_FAILURE;
            }
        }

        free(hex_buf);
        return EXIT_SUCCESS;
    }

    buffer = malloc(1 * sizeof(char));
    assert(buffer != NULL);

    instruction = malloc(instruction_size * sizeof(char));
    assert(instruction != NULL);

    repl(
        console_out,
        console_err,
        console_in,
        file,
        instruction,
        instruction_size,
        buffer,
        hex_buf,
        hex_buf_size
    );

    free(instruction);
    free(buffer);
    free(hex_buf);
    return EXIT_SUCCESS;
}

#ifdef __CloudABI__
    void program_main(const argdata_t *ad) {
        int console_out = -1;
        int console_err = -1;
        int console_in = -1;
        int root = -1;
        bool test;
        argdata_map_iterator_t ad_iter;
        const argdata_t *key_ad, *value_ad;
        const char *key;

        argdata_map_iterate(ad, &ad_iter);

        while (argdata_map_get(&ad_iter, &key_ad, &value_ad)) {
            if (argdata_get_str_c(key_ad, &key) == 0) {
                if (strcmp(key, "console_out") == 0) {
                    argdata_get_fd(value_ad, &console_out);
                } else if (strcmp(key, "console_err") == 0) {
                    argdata_get_fd(value_ad, &console_err);
                } else if (strcmp(key, "console_in") == 0) {
                    argdata_get_fd(value_ad, &console_in);
                } else if (strcmp(key, "root") == 0) {
                    argdata_get_fd(value_ad, &root);
                } else if (strcmp(key, "test") == 0) {
                    argdata_get_bool(value_ad, &test);
                }
            }

            argdata_map_next(&ad_iter);
        }

        exit(m(console_out, console_err, console_in, root, test));
#else
    void usage(int console_err, char *program) {
        dprintf(console_err, "Usage: %s [OPTIONS]\n", program);
        dprintf(console_err, "Options:\n");
        dprintf(console_err, "-t\tRun self-test\n");
        dprintf(console_err, "-h\tShow usage information\n");
    }

    int main(int argc, char **argv) {
        char *program = argv[0];
        int console_out = STDOUT_FILENO;
        int console_err = STDERR_FILENO;
        int console_in = STDIN_FILENO;
        int root = -1, status;
        bool test;

        #ifdef _MSC_VER
            size_t cwd_size = _MAX_PATH;
        #else
            size_t cwd_size = PATH_MAX;
        #endif

        FILE *cwd_file = NULL;
        char *cwd_ptr, *cwd;

        for (int i = 1; i < argc; i++) {
            char *arg = argv[i];

            if (strcmp(arg, "-h") == 0) {
                usage(console_err, program);
                return EXIT_SUCCESS;
            }

            if (strcmp(arg, "-t") == 0) {
                test = true;
            } else {
                usage(console_err, program);
                return EXIT_FAILURE;
            }
        }

        if (!test) {
            cwd = malloc(cwd_size * sizeof(char));

            #ifdef _MSC_VER
                cwd_ptr = _getcwd(cwd, cwd_size);
            #else
                cwd_ptr = getcwd(cwd, cwd_size);
            #endif

            if (!cwd_ptr) {
                dprintf(console_err, "Error getting current directory\n");
                exit(EXIT_FAILURE);
            }

            #ifdef _MSC_VER
                errno_t err = fopen_s(&cwd_file, cwd, "r");

                if (err != 0) {
                    dprintf(console_err, "Error opening current directory %s\n", cwd);
                    free(cwd);
                    exit(EXIT_FAILURE);
                }
            #else
                cwd_file = fopen(cwd, "r");
            #endif

            if (!cwd_file) {
                dprintf(console_err, "Error opening current directory %s\n", cwd);
                free(cwd);
                exit(EXIT_FAILURE);
            }

            free(cwd);

            #ifdef _MSC_VER
                root = _fileno(cwd_file);
            #else
                root = fileno(cwd_file);
            #endif
        }

        status = m(console_out, console_err, console_in, root, test);

        if (cwd_file) {
            fclose(cwd_file);
        }

        return status;
#endif
    }

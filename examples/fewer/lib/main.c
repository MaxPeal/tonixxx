// Copyright 2017 Andrew Pennebaker

#ifdef __CloudABI__
    #include <argdata.h>
    #include <program.h>
#else
    #define _GNU_SOURCE
    #include <limits.h>
#endif

#ifdef _MSC_VER
    #include <direct.h>
    #include <io.h>
#else
    #include <unistd.h>
#endif

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "fewer.h"
#include "main.h"

#ifdef __CloudABI__
    void program_main(const argdata_t *ad) {
        argdata_map_iterator_t ad_iter;
        const argdata_t *key_ad, *value_ad;
        const char *key = NULL;
        int status;
        fewer_config *config = new_fewer_config();

        argdata_map_iterate(ad, &ad_iter);

        while (argdata_map_get(&ad_iter, &key_ad, &value_ad)) {
            if (argdata_get_str_c(key_ad, &key) == 0) {
                if (strcmp(key, "stderr") == 0) {
                    argdata_get_fd(value_ad, &config->console_err);

                    //
                    // Fix assert()
                    //

                    FILE *f = fdopen(config->console_err, "w");
                    if (f) {
                        fswap(f, stderr);
                        (void) fclose(f);
                    } else {
                        dprintf(config->console_err, "Error setting stderr\n");
                        free(config);
                        exit(EXIT_FAILURE);
                    }
                } else if (strcmp(key, "stdout") == 0) {
                    argdata_get_fd(value_ad, &config->console_out);
                } else if (strcmp(key, "stdin") == 0) {
                    argdata_get_fd(value_ad, &config->console_in);
                } else if (strcmp(key, "root") == 0) {
                    argdata_get_fd(value_ad, &config->root);
                } else if (strcmp(key, "test") == 0) {
                    argdata_get_bool(value_ad, &config->test);
                }
            }

            argdata_map_next(&ad_iter);
        }

        status = repl(config);

        destroy_fewer_config(config);
        exit(status);
    }
#else
    void usage(int fd, char *program) {
        dprintf(fd, "Usage: %s [OPTIONS]\n", program);
        dprintf(fd, "Options:\n");
        dprintf(fd, "-t\tRun self-test\n");
        dprintf(fd, "-h\tShow usage information\n");
    }

    int main(int argc, char **argv) {
        char *program = argv[0];
        int status;
        fewer_config *config = new_fewer_config();
        config->console_err = STDERR_FILENO;
        config->console_out = STDOUT_FILENO;
        config->console_in = STDIN_FILENO;

        #ifdef _MSC_VER
            size_t cwd_size = _MAX_PATH;
        #else
            size_t cwd_size = PATH_MAX;
        #endif

        FILE *cwd_file = NULL;
        char *cwd_ptr, *cwd = NULL;

        for (int i = 1; i < argc; i++) {
            char *arg = argv[i];

            if (strcmp(arg, "-h") == 0) {
                usage(config->console_err, program);

                destroy_fewer_config(config);
                return EXIT_SUCCESS;
            }

            if (strcmp(arg, "-t") == 0) {
                config->test = true;
            } else {
                usage(config->console_err, program);

                destroy_fewer_config(config);
                destroy_fewer_config(config);
                return EXIT_FAILURE;
            }
        }

        if (!config->test) {
            cwd = malloc(cwd_size * sizeof(char));

            #ifdef _MSC_VER
                cwd_ptr = _getcwd(cwd, cwd_size);
            #else
                cwd_ptr = getcwd(cwd, cwd_size);
            #endif

            if (!cwd_ptr) {
                dprintf(config->console_err, "Error getting current directory\n");

                free(cwd);
                destroy_fewer_config(config);
                return EXIT_FAILURE;
            }

            #ifdef _MSC_VER
                errno_t err = fopen_s(&cwd_file, cwd, "r");

                if (err != 0) {
                    dprintf(config->console_err, "Error opening current directory %s\n", cwd);

                    free(cwd);
                    destroy_fewer_config(config);
                    return EXIT_FAILURE;
                }
            #else
                cwd_file = fopen(cwd, "r");

                if (!cwd_file) {
                    dprintf(config->console_err, "Error opening current directory %s\n", cwd);

                    free(cwd);
                    destroy_fewer_config(config);
                    return EXIT_FAILURE;
                }
            #endif

            if (!cwd_file) {
                dprintf(config->console_err, "Error opening current directory %s\n", cwd);

                free(cwd);
                destroy_fewer_config(config);
                return EXIT_FAILURE;
            }

            free(cwd);

            #ifdef _MSC_VER
                config->root = _fileno(cwd_file);
            #else
                config->root = fileno(cwd_file);
            #endif
        }

        status = repl(config);

        if (cwd_file) {
            fclose(cwd_file);
        }

        destroy_fewer_config(config);
        return status;
    }
#endif

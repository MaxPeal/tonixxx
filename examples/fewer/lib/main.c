// Copyright 2017 Andrew Pennebaker

#include "main.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__CloudABI__)
    #include <argdata.h>
    #include <program.h>
#else
    #if defined(_MSC_VER)
        #include <direct.h>
        #include <io.h>
    #else
        #include <unistd.h>
    #endif
#endif

#include "fewer.h"

#if defined(__CloudABI__)
    void program_main(const argdata_t *ad) {
        argdata_map_iterator_t ad_iter;
        const argdata_t *key_ad, *value_ad;
        const char *key = NULL;
        int
            console_err_fd = -1,
            console_out_fd = -1,
            console_in_fd = -1,
            root = -1,
            repl_status;
        FILE
            *console_err = NULL,
            *console_out = NULL,
            *console_in = NULL;
        bool test = false;

        argdata_map_iterate(ad, &ad_iter);

        while (argdata_map_get(&ad_iter, &key_ad, &value_ad)) {
            if (argdata_get_str_c(key_ad, &key) == 0) {
                if (strcmp(key, "stderr") == 0) {
                    argdata_get_fd(value_ad, &console_err_fd);

                    //
                    // Fix assert(), perror(), etc.
                    //

                    FILE *f2 = fdopen(console_err_fd, "w");

                    if (f2 == NULL) {
                        perror(NULL);
                        exit(EXIT_FAILURE);
                    }

                    fswap(f2, stderr);

                    if (fclose(f2) == EOF) {
                        exit(EXIT_FAILURE);
                    }

                    console_err = stderr;
                } else if (strcmp(key, "stdout") == 0) {
                    argdata_get_fd(value_ad, &console_out_fd);

                    console_out = fdopen(console_out_fd, "w");

                    if (console_out == NULL) {
                        perror(NULL);
                        exit(EXIT_FAILURE);
                    }
                } else if (strcmp(key, "stdin") == 0) {
                    argdata_get_fd(value_ad, &console_in_fd);

                    console_in = fdopen(console_in_fd, "r");

                    if (console_in == NULL) {
                        perror(NULL);
                        exit(EXIT_FAILURE);
                    }
                } else if (strcmp(key, "root") == 0) {
                    argdata_get_fd(value_ad, &root);
                } else if (strcmp(key, "test") == 0) {
                    argdata_get_bool(value_ad, &test);
                }
            }

            argdata_map_next(&ad_iter);
        }

        fewer_config *config = new_fewer_config(
            console_err,
            console_out,
            console_in,
            root,
            test
        );
        if (config == NULL) {
            perror(NULL);
            exit(EXIT_FAILURE);
        }

        repl_status = repl(config);
        destroy_fewer_config(config);
        exit(repl_status);
    }
#else
    void usage(char *program) {
        fprintf(stderr, "Usage: %s [OPTIONS]\n", program);
        fprintf(stderr, "Options:\n");
        fprintf(stderr, "-t\tRun self-test\n");
        fprintf(stderr, "-h\tShow usage information\n");
    }

    int main(int argc, char **argv) {
        bool test = false;
        int repl_status, i, root = -1;
        char *program = argv[0];

        for (i = 1; i < argc; i++) {
            char *arg = argv[i];

            if (strcmp(arg, "-h") == 0) {
                usage(program);
                return EXIT_SUCCESS;
            }

            if (strcmp(arg, "-t") == 0) {
                test = true;
                continue;
            }

            usage(program);
            return EXIT_FAILURE;
        }

        if (!test) {
            size_t cwd_size = _XOPEN_PATH_MAX;
            char *cwd_ptr, *cwd = malloc(cwd_size * sizeof(char));
            if (!cwd) {
                perror(NULL);
                return EXIT_FAILURE;
            }

            #if defined(_MSC_VER)
                cwd_ptr = _getcwd(cwd, cwd_size);
            #else
                cwd_ptr = getcwd(cwd, cwd_size);
            #endif

            if (cwd_ptr == NULL) {
                perror(NULL);
                free(cwd);
                return EXIT_FAILURE;
            }

            #if defined(_MSC_VER)
                int fd = (int) CreateFileA(
                    cwd,
                    GENERIC_READ,
                    FILE_SHARE_READ |
                        FILE_SHARE_DELETE,
                    NULL,
                    OPEN_EXISTING,
                    FILE_FLAG_BACKUP_SEMANTICS,
                    NULL
                );

                if (fd == -1) {
                    DWORD err = GetLastError();
                    char *errStr = NULL;
                    FormatMessageA(
                        FORMAT_MESSAGE_ALLOCATE_BUFFER |
                            FORMAT_MESSAGE_FROM_SYSTEM |
                            FORMAT_MESSAGE_IGNORE_INSERTS,
                        NULL,
                        err,
                        0,
                        (LPSTR) &errStr,
                        0,
                        NULL
                    );

                    fprintf(stderr, "%s\n", errStr);
                    LocalFree(errStr);
                    free(cwd);
                    return EXIT_FAILURE;
                }

                root = fd;
            #else
                FILE *cwd_file = fopen(cwd, "r");

                if (cwd_file == NULL) {
                    perror(NULL);
                    free(cwd);
                    return EXIT_FAILURE;
                }

                root = fileno(cwd_file);
            #endif

            if (root == -1) {
                perror(NULL);
                free(cwd);
                return EXIT_FAILURE;
            }

            free(cwd);
        }

        fewer_config *config = new_fewer_config(
            stderr,
            stdout,
            stdin,
            root,
            test
        );
        if (config == NULL) {
            perror(NULL);
            return EXIT_FAILURE;
        }

        repl_status = repl(config);
        destroy_fewer_config(config);
        return repl_status;
    }
#endif

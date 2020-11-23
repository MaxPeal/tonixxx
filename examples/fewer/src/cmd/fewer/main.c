/**
 * @copyright 2020 YelloSoft
 */

#include "main.h"

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__CloudABI__)
#include <argdata.h>
#include <program.h>
#elif defined(_MSC_VER)
#include <direct.h>
#include <io.h>
#else
#include <unistd.h>
#endif

#include "fewer/fewer.h"

#if defined(_XOPEN_PATH_MAX)
#define X_PATH_MAX _XOPEN_PATH_MAX
#elif defined(MAXPATHLEN)
#define X_PATH_MAX MAXPATHLEN
#elif defined(_POSIX_PATH_MAX)
#define X_PATH_MAX _POSIX_PATH_MAX
#elif defined(_MAX_PATH)
#define X_PATH_MAX _MAX_PATH
#elif defined(_PC_PATH_MAX)
#define X_PATH_MAX _PC_PATH_MAX
#elif defined(PATH_MAX)
#define X_PATH_MAX PATH_MAX
#else
#define X_PATH_MAX 1024
#endif

static const char *PROMPT = "> ";

static int show_commands(FILE *console) {
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

static void chomp(char *s, size_t length) {
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

#if defined(_MSC_VER)
static int fchdir(int fd) {
    DWORD result;
    char base_path[X_PATH_MAX];

    result = GetFinalPathNameByHandleA(
        (HANDLE) fd,
        base_path,
        sizeof(base_path)/sizeof(base_path[0]) - 1,
        FILE_NAME_NORMALIZED
    );

    if (result > sizeof(base_path)/sizeof(base_path[0])) {
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

#if defined(_MSC_VER) || defined(__MirBSD__) || defined(__minix)
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

static bool validate_fewer_config(fewer_config *config) {
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

static int unit_test(fewer_config *config) {
    char hex_buf[3];

    for (short d, c = 0; c < 0x100; c++) {
        render_boi(config->console_err, (unsigned int) c, &hex_buf[0], sizeof(hex_buf)/sizeof(hex_buf[0]));
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

static int repl(fewer_config *config) {
    int fd, c;
    FILE *f = NULL;
    char hex_buf[3], instruction[X_PATH_MAX + 2], command = '\0', *content = NULL;

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

        if (fgets(instruction, sizeof(instruction)/sizeof(instruction[0]), config->console_in) == NULL) {
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

                render_boi(config->console_err, (unsigned int) c, &hex_buf[0], sizeof(hex_buf)/sizeof(hex_buf[0]));
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

                if (strlen(content) > sizeof(hex_buf)/sizeof(hex_buf[0]) - 1) {
                    if (show_commands(config->console_err) == EOF) {
                        return EXIT_FAILURE;
                    }

                    break;
                }

#if defined(_MSC_VER)
                strncpy_s(hex_buf, sizeof(hex_buf)/sizeof(hex_buf[0]), content, strlen(content));
#else
                strncpy(hex_buf, content, sizeof(hex_buf)/sizeof(hex_buf[0]));
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

#if defined(__CloudABI__)
void program_main(const argdata_t *ad) {
    argdata_map_iterator_t ad_iter;
    const argdata_t *key_ad, *value_ad;
    const char *key = NULL;
    int
        console_err_fd = -1,
        console_out_fd = -1,
        console_in_fd = -1,
        root = -1;
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

                errno = 0;
                FILE *f2 = fdopen(console_err_fd, "w");

                if (f2 == NULL) {
                    exit(EXIT_FAILURE);
                }

                fswap(f2, stderr);

                errno = 0;
                if (fclose(f2) == EOF) {
                    perror(NULL);
                    exit(EXIT_FAILURE);
                }

                console_err = stderr;
            } else if (strcmp(key, "stdout") == 0) {
                argdata_get_fd(value_ad, &console_out_fd);

                errno = 0;
                console_out = fdopen(console_out_fd, "w");

                if (console_out == NULL) {
                    perror(NULL);
                    exit(EXIT_FAILURE);
                }
            } else if (strcmp(key, "stdin") == 0) {
                argdata_get_fd(value_ad, &console_in_fd);

                errno = 0;
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

    fewer_config *config = &(fewer_config) {
        .console_err = console_err,
        .console_out = console_out,
        .console_in = console_in,
        .root = root,
        .test = (int) test
    };

    if (config->test) {
        exit(unit_test(config));
    }

    exit(repl(config));
}
#else
void usage(char **argv) {
    fprintf(stderr, "Usage: %s [OPTIONS]\n", argv[0]);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "-t\tRun self-test\n");
    fprintf(stderr, "-h\tShow usage information\n");
}

int main(int argc, char **argv) {
    bool test = false;
    int i, root = -1;

    for (i = 1; i < argc; i++) {
        char *arg = argv[i];

        if (strcmp(arg, "-h") == 0) {
            usage(argv);
            return EXIT_SUCCESS;
        }

        if (strcmp(arg, "-t") == 0) {
            test = true;
            continue;
        }

        usage(argv);
        return EXIT_FAILURE;
    }

    if (!test) {
        char cwd[X_PATH_MAX];
        char *cwd_ptr;

        errno = 0;

#if defined(_MSC_VER)
        cwd_ptr = _getcwd(cwd, X_PATH_MAX);
#else
        cwd_ptr = getcwd(cwd, X_PATH_MAX);
#endif

        if (cwd_ptr == NULL) {
            perror(NULL);
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
            char *err_msg = NULL;
            FormatMessageA(
                FORMAT_MESSAGE_ALLOCATE_BUFFER |
                    FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL,
                err,
                0,
                (LPSTR) &err_msg,
                0,
                NULL
            );

            fprintf(stderr, "%s\n", err_msg);
            LocalFree(err_msg);
            return EXIT_FAILURE;
        }

        root = fd;
#else
        errno = 0;
        FILE *cwd_file = fopen(cwd, "r");

        if (cwd_file == NULL) {
            perror(NULL);
            return EXIT_FAILURE;
        }

        errno = 0;
        root = fileno(cwd_file);
#endif

        if (root == -1) {
            perror(NULL);
            return EXIT_FAILURE;
        }
    }

    fewer_config *config = &(fewer_config) {
        .console_err = stderr,
        .console_out = stdout,
        .console_in = stdin,
        .root = root,
        .test = test
    };

    if (config->test) {
        return unit_test(config);
    }

    return repl(config);
}
#endif

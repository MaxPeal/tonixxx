/**
 * @copyright 2020 YelloSoft
 */

#include "main.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if defined(__APPLE__)
#include <sys/syslimits.h>
#else
#include <limits.h>
#endif

#if defined(_MSC_VER)
#include <direct.h>
#include <io.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "fewer/fewer.h"

#if defined(_MSC_VER)
#define PATH_MAX MAX_PATH
#endif

static const char *PROMPT = "> ";

static void show_commands(FILE *console) {
    fprintf(console, "l <path>\tLoad file\n");
    fprintf(console, "n\t\tShow next byte\n");
    fprintf(console, "r <hex pair>\tRender an input byte\n");
    fprintf(console, "q\t\tQuit\n");
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

static int unit_test() {
    char hex_buf[3];

    for (int d, c = 0; c < 0x100; c++) {
        if (render_boi(hex_buf, c) != 0) {
            fprintf(stderr, "error during parsing\n");
            return EXIT_FAILURE;
        }

        d = parse_boi(hex_buf);

        if (d < 0) {
            fprintf(stderr, "error during formatting\n");
            return EXIT_FAILURE;
        }

        if (d != c) {
            fprintf(stderr, "got: %c (0x%02x), expected: %c (0x%02x)\n", d, d, c, c);
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}

static int repl(fewer_config *config) {
    int c;
    FILE *f = NULL;
    char hex_buf[3], instruction[PATH_MAX + 2], command = '\0', *content = NULL;

    if (!validate_fewer_config(config)) {
        return EXIT_FAILURE;
    }

    while (true) {
        fprintf(config->console_out, "%s", PROMPT);

        if (fgets(instruction, sizeof(instruction)/sizeof(instruction[0]), config->console_in) == NULL) {
            if (ferror(config->console_in) != 0) {
                return EXIT_FAILURE;
            }

            return EXIT_SUCCESS;
        }

        chomp(instruction, strlen(instruction));

        if (strlen(instruction) == 0) {
            show_commands(config->console_err);
            continue;
        }

        command = instruction[0];

        switch (command) {
            case 'l':
                content = strchr(instruction, ' ');

                if (content == NULL || strlen(content) < 2) {
                    show_commands(config->console_err);
                    break;
                }

                content++;

                if (f != NULL && fclose(f) == EOF) {
                    fprintf(config->console_err, "error closing file\n");
                    return EXIT_FAILURE;
                }

#if defined(_MSC_VER)
                 if (fopen_s(&f, content, "rb") != 0) {
                    fprintf(stderr, "error opening file %s\n", content);
                    return EXIT_FAILURE;
                }
#else
                f = fopen(content, "rb");

                if (f == NULL) {
                    fprintf(stderr, "error opening file %s\n", content);
                    return EXIT_FAILURE;
                }
#endif

                break;
            case 'n':
                if (f == NULL) {
                    fprintf(config->console_err, "no file loaded\n");
                    break;
                }

                errno = 0;
                c = fgetc(f);

                if (c == EOF) {
                    if (ferror(f) != 0) {
                        fprintf(config->console_err, "error reading character from file\n");
                        return EXIT_FAILURE;
                    }

                    return EXIT_SUCCESS;
                }

                render_boi(hex_buf, c);
                fprintf(config->console_out, "%s\n", hex_buf);
                break;
            case 'r':
                content = strchr(instruction, ' ');

                if (content == NULL || strlen(content) < 2) {
                    show_commands(config->console_err);
                    break;
                }

                content++;

                if (strlen(content) > sizeof(hex_buf)/sizeof(hex_buf[0]) - 1) {
                    show_commands(config->console_err);
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
                    break;
                }

                fprintf(config->console_out, "%c\n", c);
                break;
            case 'q':
                if (f != NULL && fclose(f) == EOF) {
                    fprintf(config->console_err, "Error closing file\n");
                    return EXIT_FAILURE;
                }

                return EXIT_SUCCESS;
            default:
                show_commands(config->console_err);
        }
    }
}

static void usage(char **argv) {
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
        char cwd[PATH_MAX];
        char *cwd_ptr;

        errno = 0;

#if defined(_MSC_VER)
        cwd_ptr = _getcwd(cwd, PATH_MAX);
#else
        cwd_ptr = getcwd(cwd, PATH_MAX);
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

        if (root == -1) {
            perror(NULL);
            return EXIT_FAILURE;
        }
#endif
    }

    fewer_config *config = &(fewer_config) {
        .console_err = stderr,
        .console_out = stdout,
        .console_in = stdin,
        .root = root,
        .test = test
    };

    if (config->test) {
        return unit_test();
    }

    return repl(config);
}

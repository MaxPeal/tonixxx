/**
 * @copyright 2020 YelloSoft
 */

#include <limits.h>
#if defined(__APPLE__)
#include <sys/syslimits.h>
#elif defined(_MSC_VER)
#include <windows.h>
#define PATH_MAX MAX_PATH
#endif

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fewer/fewer.h"

static void show_commands(void) {
    fprintf(stderr, "l <path>\tLoad file\n");
    fprintf(stderr, "n\t\tShow next byte\n");
    fprintf(stderr, "p <hex pair>\tParse a hexpair\n");
    fprintf(stderr, "q\t\tQuit\n");
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

static int unit_test(void) {
    char hex_buf[3];

    for (int c = 0; c < UCHAR_MAX; c++) {
        if (render_boi(hex_buf, (unsigned char) c) < 0) {
            fprintf(stderr, "encoding error\n");
            return EXIT_FAILURE;
        }

        const int d = parse_boi(hex_buf);

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

static int repl(void) {
    int c = 0;
    FILE *f = NULL;
    char hex_buf[3], instruction[PATH_MAX + 2], command = '\0', *content = NULL;
    const size_t hex_buf_sz = sizeof(hex_buf), hex_buf_len = hex_buf_sz - 1, instruction_sz = sizeof(instruction);
    const char *PROMPT = "> ";

    while (true) {
        printf("%s", PROMPT);

        if (fgets(instruction, instruction_sz, stdin) == NULL) {
            if (ferror(stdin) != 0) {
                return EXIT_FAILURE;
            }

            return EXIT_SUCCESS;
        }

        chomp(instruction, strlen(instruction));

        if (strlen(instruction) == 0) {
            show_commands();
            continue;
        }

        command = instruction[0];

        switch (command) {
        case 'l':
            content = strchr(instruction, ' ');

            if (content == NULL || strlen(content) < 2) {
                show_commands();
                break;
            }

            content++;

            if (f != NULL && fclose(f) == EOF) {
                fprintf(stderr, "error closing file\n");
                return EXIT_FAILURE;
            }

#if defined(_MSC_VER)
            if (fopen_s(&f, content, "rb") != 0) {
                fprintf(stderr, "error opening file %s\n", content);
                return EXIT_FAILURE;
            }
#else
            f = fopen(content, "rbe");

            if (f == NULL) {
                fprintf(stderr, "error opening file %s\n", content);
                return EXIT_FAILURE;
            }
#endif

            break;
        case 'n':
            if (f == NULL) {
                fprintf(stderr, "no file loaded\n");
                break;
            }

            c = fgetc(f);

            if (c == EOF) {
                if (ferror(f) != 0) {
                    fprintf(stderr, "error reading character from file\n");
                    return EXIT_FAILURE;
                }

                return EXIT_SUCCESS;
            }

            if (render_boi(hex_buf, (unsigned char) c) < 0) {
                fprintf(stderr, "encoding error\n");
                return EXIT_FAILURE;
            }

            printf("%s\n", hex_buf);
            break;
        case 'p':
            content = strchr(instruction, ' ');

            if (content == NULL || strlen(content) < 2) {
                show_commands();
                break;
            }

            content++;

            if (strlen(content) > hex_buf_len) {
                show_commands();
                break;
            }

            (void) snprintf(hex_buf, hex_buf_sz, "%s", content);
            c = parse_boi(hex_buf);

            if (c == -1) {
                fprintf(stderr, "error parsing hexadecimal %s\n", hex_buf);
                break;
            }

            printf("%c\n", c);
            break;
        case 'q':
            if (f != NULL && fclose(f) == EOF) {
                fprintf(stderr, "error closing file\n");
                return EXIT_FAILURE;
            }

            return EXIT_SUCCESS;
        default:
            show_commands();
        }
    }
}

static void usage(const char **argv) {
    fprintf(stderr, "Usage: %s [OPTIONS]\n", argv[0]);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "-t\tRun self-test\n");
    fprintf(stderr, "-h\tShow usage information\n");
}

int main(int argc, const char **argv) {
    bool test = false;

    for (int i = 1; i < argc; i++) {
        const char *arg = argv[i];

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

    if (test) {
        return unit_test();
    }

    return repl();
}

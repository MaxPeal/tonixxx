/**
 * @copyright 2020 YelloSoft
 */

#include <errno.h>
#include <limits.h>
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

#if defined(_MSC_VER) || defined(__MirBSD__) || defined(__minix)
#include <stdarg.h>
#endif

#include "fewer/fewer.h"

void render_boi(unsigned int b, /*@out@*/ char *s, size_t s_len) {
    int write_count = snprintf(s, s_len, "%02x", b);

    if (write_count < 0) {
        fprintf(stderr, "error during encoding\n");
    } else if ((size_t) write_count > s_len - 1) {
        fprintf(stderr, "buffer requires %d character allocation to render value\n", write_count);
    }
}

short parse_boi(char *s) {
    char *endptr = s;
    unsigned long int n = strtoul(s, &endptr, 16);

    if (endptr == s) {
        return -1;
    }

    if (n == ULONG_MAX) {
        return -1;
    }

    if (n > UINT_MAX) {
        return -1;
    }

    return (short) n;
}

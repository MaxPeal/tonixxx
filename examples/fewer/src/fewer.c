/**
 * @copyright 2020 YelloSoft
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fewer/fewer.h"

int render_boi(char *hexpair, unsigned int b) {
    size_t pair_sz = 3;
    int bytes_written = snprintf(hexpair, pair_sz, "%02x", b);

    if (bytes_written < 0 || (size_t) bytes_written > pair_sz) {
        return -1;
    }

    return 0;
}

int parse_boi(char *hexpair) {
    char pair[3];
    size_t pair_sz = sizeof(pair);
    int bytes_written = snprintf(pair, pair_sz, "%.*s", 2, hexpair);

    if (bytes_written < 0 || (size_t) bytes_written > pair_sz) {
        return -1;
    }

    errno = 0;
    int n = (int) strtol(pair, NULL, 16);

    if (errno != 0) {
        return -1;
    }

    return n;
}

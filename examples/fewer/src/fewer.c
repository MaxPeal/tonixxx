/**
 * @copyright 2020 YelloSoft
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fewer/fewer.h"

int render_boi(char *hexpair, unsigned int b) {
    size_t sz = 3;
    int write_count = snprintf(hexpair, sz, "%02x", b);

    if (write_count < 0 || (size_t) write_count > sz) {
        return -1;
    }

    return 0;
}

int parse_boi(char *hexpair) {
    char pair[3];
    size_t pair_sz = sizeof(pair);
    memcpy(pair, hexpair, pair_sz - 1);
    pair[pair_sz - 1] = '\0';

    errno = 0;
    int n = (int) strtol(pair, NULL, 16);

    if (errno != 0) {
        return -1;
    }

    return n;
}

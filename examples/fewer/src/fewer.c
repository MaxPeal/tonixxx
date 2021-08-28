/**
 * @copyright 2020 YelloSoft
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "fewer/fewer.h"

int render_boi(char *hexpair, unsigned char c) {
    return snprintf(hexpair, 3, "%02x", c);
}

int parse_boi(const char *hexpair) {
    if (hexpair[2] != '\0') {
        return -1;
    }

    errno = 0;
    const int c = (int) strtol(hexpair, NULL, 16);

    if (errno != 0) {
        return -1;
    }

    return c;
}

/**
 * @copyright 2020 YelloSoft
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "fewer/fewer.h"

void render_boi(char *hexpair, unsigned char c) {
    (void) snprintf(hexpair, 3, "%02x", c);
}

int parse_boi(char *hexpair) {
    if (hexpair[2] != '\0') {
        return -1;
    }

    errno = 0;
    int c = (int) strtol(hexpair, NULL, 16);

    if (errno != 0) {
        return -1;
    }

    return c;
}

#pragma once

/**
 * @copyright 2020 YelloSoft
 */

#if defined(__SVR4)
#define _POSIX_C_SOURCE 200809L
#endif

#include <stdio.h>

typedef struct {
    int root;
    int test;
    FILE *console_err;
    FILE *console_out;
    FILE *console_in;
} fewer_config;

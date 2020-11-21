#pragma once

/**
 * @copyright 2020 YelloSoft
 */

#if defined(__SVR4)
#define _POSIX_C_SOURCE 200809L
#endif

#include <stdio.h>

/**
 * fewer_config parameterizes a session.
 */
typedef struct {
    /** root denotes the session base file path. */
    int root;

    /** test denots whether to execute self-tests. */
    int test;

    /** console_err denotes the stderr stream. */
    FILE *console_err;

    /** console_out denotes the stdout stream. */
    FILE *console_out;

    /** console_in denotes the stdin stream. */
    FILE *console_in;
} fewer_config;

void usage(char **argv);

#pragma once

/**
 * @copyright 2020 YelloSoft
 * @mainpage
 *
 * @ref parse_boi reads hexadecimal pairs.
 *
 * @ref render_boi formats hexadecimal pairs.
 */

#if defined(__sun)
#define __EXTENSIONS__
#elif defined(_MSC_VER)
#include <windows.h>
typedef int mode_t;
#endif

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>

#if defined(__MirBSD__)
#include <sys/param.h>
#endif

/**
 * render_boi formats hexadecimal pairs.
 *
 * @param console error stream
 * @param b hexpair pair
 * @param s hexadecimal pair buffer
 * @param s_len buffer length
 */
void render_boi(FILE *console, unsigned int b, /*@out@*/ char *s, size_t s_len);

/**
 * parse_boi reads hexadecimal pairs.
 *
 * @param s hexadecimal pair
 *
 * @returns value. -1 indicates an error.
 */
short parse_boi(char *s);

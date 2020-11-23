#pragma once

/**
 * @copyright 2020 YelloSoft
 * @mainpage
 *
 * @ref parse_boi reads hexadecimal pairs.
 *
 * @ref render_boi formats hexadecimal pairs.
 */

/**
 * render_boi formats hexadecimal pairs.
 *
 * @param s hexadecimal pair buffer
 * @param s_len buffer length
 * @param b hexpair pair
 */
void render_boi(char *s, size_t s_len, unsigned int b);

/**
 * parse_boi reads hexadecimal pairs.
 *
 * @param s hexadecimal pair
 *
 * @returns value. -1 indicates an error.
 */
short parse_boi(char *s);

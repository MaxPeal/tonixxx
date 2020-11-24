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
 * render_boi formats a hexadecimal pair.
 *
 * @param hexpair buffer (3 bytes)
 * @param b value
 *
 * @returns zero. -1 indicates error.
 */
int render_boi(char *hexpair, unsigned int b);

/**
 * parse_boi reads a hexadecimal pair.
 *
 * @param hexpair hexadecimal pair (3 bytes)
 *
 * @returns value. -1 indicates an error.
 */
int parse_boi(char *hexpair);

#pragma once

/**
 * @copyright 2020 YelloSoft
 * @mainpage
 *
 * @ref pal analyzes text.
 */

#include <string>

/**
 * @brief pal provides text analyzers.
 */
namespace pal {
    /**
     * @brief Palindrome examines text for symmetry.
     *
     * @param s text
     *
     * @returns whether the string is a palindrome.
     */
    bool Palindrome(const std::string &s);
}

/**
 * @copyright 2020 YelloSoft
 */

#include <algorithm>
#include <string>

#include "palindrome/palindrome.hpp"

bool pal::Palindrome(const std::string &s) {
    std::string s2(s);
    std::reverse(s2.begin(), s2.end());
    return s2 == s;
}

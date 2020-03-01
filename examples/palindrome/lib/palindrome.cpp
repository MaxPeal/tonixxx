// Copyright 2018 Andrew Pennebaker

#include "palindrome.hh"

#include <algorithm>
#include <string>

using std::string;

bool palindrome(string s) {
    string s_copy(s);
    std::reverse(s_copy.begin(), s_copy.end());
    return s == s_copy;
}

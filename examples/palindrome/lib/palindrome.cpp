// Copyright 2018 Andrew Pennebaker

#include <string>
#include <algorithm>

#include "palindrome.h"

bool palindrome(std::string s) {
  std::string s_copy(s);
  std::reverse(s_copy.begin(), s_copy.end());

  return s == s_copy;
}

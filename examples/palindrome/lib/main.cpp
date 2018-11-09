// Copyright 2018 Andrew Pennebaker

#include <vector>
#include <sstream>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <iomanip>
#include <iostream>

#include "main.h"
#include "palindrome.h"

void usage(char* program) {
  std::cout << "Usage: " << program << "[-t]" << std::endl;
}

void test() {
  std::vector<int> palindromes;

  for (int x = 0; x < 256; x++) {
    std::stringstream x_hex;

    x_hex << std::setfill('0') << std::setw(2) << std::hex << x;

    if (palindrome(x_hex.str())) {
      palindromes.push_back(x);
    }
  }

  assert(palindromes.size() == 16);
}

int main(int argc, char** argv) {
  std::string line;

  if (argc > 2) {
    usage(argv[0]);
    return EXIT_FAILURE;
  }

  if (argc > 1 && strcmp(argv[1], "-t") == 0) {
    test();
    return EXIT_SUCCESS;
  }

  while (!std::cin.bad() && !std::cin.eof()) {
    std::getline(std::cin, line);

    std::cout << "Palindrome: " << palindrome(line) << std::endl;
  }

  if (std::cin.bad()) {
    return EXIT_FAILURE;
  } else {
    return EXIT_SUCCESS;
  }
}

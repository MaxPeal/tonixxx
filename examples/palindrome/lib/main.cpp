// Copyright 2018 Andrew Pennebaker

#include "main.hh"

#include <cstdlib>
#include <cstring>

#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "palindrome.hh"

using std::cin;
using std::cout;
using std::endl;

void usage(char **argv) {
    cout << "Usage: " << argv[0] << "[-t]" << endl;
}

int test() {
    std::vector<int> palindromes;

    for (int x = 0; x < 256; x++) {
        std::stringstream x_hex;
        x_hex << std::setfill('0') << std::setw(2) << std::hex << x;

        if (palindrome(x_hex.str())) {
            palindromes.push_back(x);
        }
    }

    if (palindromes.size() != 16) {
        std::cerr << "Expected to find 16 palindromes, instead found " << palindromes.size() << endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

int main(int argc, char **argv) {
    std::string line;

    if (argc > 2) {
        usage(argv);
        return EXIT_FAILURE;
    }

    if (argc > 1 && strcmp(argv[1], "-t") == 0) {
        return test();
    }

    while (!cin.bad() && !cin.eof()) {
        std::getline(cin, line);
        cout << "Palindrome: " << palindrome(line) << endl;
    }

    if (cin.bad()) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

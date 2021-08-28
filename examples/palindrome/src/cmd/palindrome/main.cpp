/**
 * @copyright 2020 YelloSoft
 */

#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "palindrome/palindrome.hpp"

static void Usage(std::vector<std::string_view> args) {
    std::cout << "Usage: " << args.front() << " [OPTIONS]" << std::endl
              << std::endl
              << "-t\tSelf test" << std::endl
              << "-h\tShow usage information" << std::endl;
}

static int Test() {
    std::vector<int> palindromes;

    for (int x = 0; x < 256; x++) {
        std::stringstream x_hex;
        x_hex << std::setfill('0') << std::setw(2) << std::hex << x;

        if (pal::Palindrome(x_hex.str())) {
            palindromes.push_back(x);
        }
    }

    if (palindromes.size() != 16) {
        std::cerr << "Expected: 16 palindromes, got: " << palindromes.size() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

int main(int argc, char **argv) {
    const auto args = std::vector<std::string_view>{ argv, argv + argc };

    if (args.size() > 2) {
        Usage(args);
        return EXIT_FAILURE;
    }

    if (args.size() > 1 && args.at(1).compare("-h") == 0) {
        Usage(args);
        return EXIT_SUCCESS;
    }

    if (args.size() > 1 && args.at(1).compare("-t") == 0) {
        return Test();
    }

    std::string line;

    while (!std::cin.bad() && !std::cin.eof()) {
        std::getline(std::cin, line);

        if (!std::cin) {
            break;
        }

        std::cout << "Palindrome: " << pal::Palindrome(line) << std::endl;
    }

    if (std::cin.bad()) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

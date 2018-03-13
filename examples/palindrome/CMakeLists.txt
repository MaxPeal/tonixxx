cmake_minimum_required(VERSION 3.4)

project(palindrome)
set(EXECUTABLE_OUTPUT_PATH "bin")
set(CMAKE_CXX_FLAGS "-std=c++11 -O2 -Wall -Wextra -pedantic -Weffc++ -Wno-c++98-compat")
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wmost -Weverything")
endif()
add_executable(palindrome lib/main.cpp lib/palindrome.cpp)

enable_testing()
add_test(NAME palindrometest COMMAND palindrome -t)

add_custom_target(cppcheck COMMAND cppcheck -q --enable=all --suppressions-list=suppressions.cfg lib)
add_custom_target(cpplint COMMAND find lib -type f -name '*.cpp' -o -name '*.[ch]' -exec cpplint --filter=-readability/streams,-build/header_guard,-whitespace/parens,-readability/braces,-whitespace/newline,-legal/copyright {} "\;")
add_custom_target(splint COMMAND 	find lib -type f -name '*.c' -exec splint {} "\;")
add_custom_target(vera++ COMMAND find lib -type f -name '*.cpp' -o -name '*.[ch]' -exec vera++ -s {} "\;")
add_custom_target(lint DEPENDS cppcheck cpplint splint vera++)

add_custom_target(clean-cmake COMMAND find . -iwholename '*cmake*' -not -name CMakeLists.txt -delete)

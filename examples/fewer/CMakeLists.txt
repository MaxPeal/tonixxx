cmake_minimum_required(VERSION 3.4)

project(fewer)
set(EXECUTABLE_OUTPUT_PATH "bin")
set(CMAKE_C_FLAGS "-O2 -Wall -Wextra")
if (CMAKE_C_COMPILER_ID MATCHES "Clang")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wmost -Weverything")
endif()
add_executable(fewer lib/main.c lib/fewer.c lib/fewer.h)

enable_testing()
add_test(NAME fewertest COMMAND fewer -t)

add_custom_target(cppcheck COMMAND cppcheck -q --enable=all --suppressions-list=suppressions.cfg lib)
add_custom_target(splint COMMAND splint lib/main.c lib/fewer.c lib/fewer.h)
add_custom_target(vera++ COMMAND vera++ -s lib/main.c lib/fewer.c lib/fewer.h)
add_custom_target(lint DEPENDS cppcheck splint vera++)

add_custom_target(clean-cmake COMMAND find . -iwholename '*cmake*' -not -name CMakeLists.txt -delete)

cmake_minimum_required(VERSION 3.4)

project(fewer)

set(EXECUTABLE_OUTPUT_PATH "bin")

if (MSVC)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_DEBUG "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG "${EXECUTABLE_OUTPUT_PATH}")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE "${EXECUTABLE_OUTPUT_PATH}")
endif()

if (CMAKE_C_COMPILER_ID MATCHES "clang")
  set(CMAKE_C_FLAGS "-std=c11 -O2 -Wall -Wextra -Wmost -Weverything")
elseif (CMAKE_C_COMPILER_ID MATCHES "gcc")
  set(CMAKE_C_FLAGS "-std=c11 -O2 -Wall -Wextra")
else()
  set(CMAKE_C_FLAGS "-std=c11 -O2 -Wall")
endif()

add_executable(fewer lib/main.c lib/fewer.c lib/fewer.h)

enable_testing()

add_test(NAME fewertest COMMAND fewer -t)

add_custom_target(cppcheck COMMAND cppcheck -q --enable=all --suppressions-list=suppressions.cfg lib)
add_custom_target(splint COMMAND splint lib/main.c lib/fewer.c lib/fewer.h)
add_custom_target(vera++ COMMAND vera++ -s lib/main.c lib/fewer.c lib/fewer.h)

if (${CMAKE_SYSTEM_NAME} MATCHES "SmartOS" OR ${CMAKE_GNU_C_MACHINE} MATCHES "uclibc")
  add_custom_target(lint)
elseif (${CMAKE_SYSTEM_NAME} MATCHES "MINIX")
  add_custom_target(lint DEPENDS splint)
elseif(MSVC OR ${CMAKE_SYSTEM_NAME} MATCHES "Void" OR ${CMAKE_SYSTEM_NAME} MATCHES "OpenBSD")
  add_custom_target(lint DEPENDS cppcheck splint)
else()
  add_custom_target(lint DEPENDS cppcheck splint vera++)
endif()

add_custom_target(clean-cmake COMMAND find . -iwholename '*cmake*' -not -name CMakeLists.txt -delete)

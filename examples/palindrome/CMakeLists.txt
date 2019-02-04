cmake_minimum_required(VERSION 3.4)

if("$ENV{CLOUD_ABI_TRIPLET}" MATCHES "cloudabi")
    set(CMAKE_C_COMPILER "$ENV{CLOUD_ABI_TRIPLET}-cc")
    set(CMAKE_CXX_COMPILER "$ENV{CLOUD_ABI_TRIPLET}-c++")
    set(CMAKE_AR "$ENV{CLOUD_ABI_TRIPLET}-ar" CACHE FILEPATH "Archiver")
    set(CMAKE_NM "$ENV{CLOUD_ABI_TRIPLET}-nm" CACHE FILEPATH "Nm")
    set(CMAKE_RANLIB "$ENV{CLOUD_ABI_TRIPLET}-ranlib" CACHE FILEPATH "Ranlib")
    set(CMAKE_SYSTEM_NAME "Generic")
endif()

project(palindrome)

set(EXECUTABLE_OUTPUT_PATH "bin")
set(LSB_RELEASE "")

if(MSVC)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_DEBUG "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG "${EXECUTABLE_OUTPUT_PATH}")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE "${EXECUTABLE_OUTPUT_PATH}")
elseif(CMAKE_SYSTEM_NAME MATCHES "Linux")
    execute_process(COMMAND lsb_release -a OUTPUT_VARIABLE LSB_RELEASE)
endif()

if(MSVC)
    set(CMAKE_CXX_FLAGS "/O2 /W4 /wd4204")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "clang")
    set(CMAKE_CXX_FLAGS "-std=c++98 -O2 -Wall -Wextra -Wmost -Weverything")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    set(CMAKE_CXX_FLAGS "-std=c++98 -O2 -Wall -Wextra")
endif()

add_executable(palindrome lib/main.cpp lib/palindrome.cpp)

enable_testing()

add_test(NAME palindrometest COMMAND palindrome -t)

add_custom_target(cppcheck COMMAND cppcheck -q --enable=all --suppressions-list=suppressions.cfg --error-exitcode=1 lib)
add_custom_target(cpplint COMMAND sh recur-cpplint.sh)
add_custom_target(vera++ COMMAND sh recur-vera++.sh)

if(
    CMAKE_SYSTEM_NAME MATCHES "Haiku" OR
    CMAKE_SYSTEM_NAME MATCHES "MirBSD" OR
    CMAKE_SYSTEM_NAME MATCHES "SunOS" OR
    "${CMAKE_GNU_C_MACHINE}" MATCHES "uclibc"
)
    add_custom_target(lint)
elseif(
    MSVC OR
    CMAKE_SYSTEM_NAME MATCHES "OpenBSD" OR
    LSB_RELEASE MATCHES "Void"
)
    add_custom_target(lint DEPENDS cppcheck cpplint)
else()
    add_custom_target(lint DEPENDS cppcheck cpplint vera++)
endif()

add_custom_target(valgrind_test COMMAND valgrind --error-exitcode=1 --leak-check=full bin/palindrome -t)
add_custom_target(valgrind DEPENDS valgrind_test)

# Broken or missing valgrind
if(
    MSVC OR
    CMAKE_SYSTEM_NAME MATCHES "DragonFly" OR
    CMAKE_SYSTEM_NAME MATCHES "Haiku" OR
    CMAKE_SYSTEM_VERSION MATCHES "HBSD" OR
    CMAKE_SYSTEM_NAME MATCHES "SunOS" OR
    CMAKE_SYSTEM_NAME MATCHES "Darwin" OR
    CMAKE_SYSTEM_NAME MATCHES "Minix" OR
    CMAKE_SYSTEM_NAME MATCHES "MirBSD" OR
    CMAKE_SYSTEM_NAME MATCHES "NetBSD" OR
    LSB_RELEASE MATCHES "Void" OR
    CMAKE_SYSTEM_NAME MATCHES "OpenBSD" OR
    "${CMAKE_GNU_C_MACHINE}" MATCHES "uclibc"
)
add_custom_target(leaks)
else()
    add_custom_target(leaks DEPENDS valgrind)
endif()

add_custom_target(clean-cmake COMMAND sh clean-cmake.sh)

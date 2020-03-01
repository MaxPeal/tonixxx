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

set(WINDOWS_C_FLAGS "${CMAKE_C_FLAGS} /O2 /WX /W4 /wd4204")
set(WINDOWS_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2 /WX /W4 /wd4204")

set(UNIX_C_FLAGS "${CMAKE_C_FLAGS} -O2 -Werror")
set(UNIX_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2 -Werror")

set(GNU_C_FLAGS "${UNIX_C_FLAGS} -std=gnu98 -Wextra -Wall -pedantic")
set(GNU_CXX_FLAGS "${UNIX_CXX_FLAGS} -std=c++98 -Wextra -Wall -pedantic -Weffc++")

set(CLANG_C_FLAGS "${GNU_C_FLAGS} -Wmost -Weverything")
set(CLANG_CXX_FLAGS "${GNU_CXX_FLAGS} -Wmost -Weverything")

if(MSVC)
    set(CMAKE_C_FLAGS "${WINDOWS_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${WINDOWS_CXX_FLAGS}")
elseif("$ENV{CLOUD_ABI_TRIPLET}" MATCHES "cloudabi")
    set(CMAKE_C_FLAGS "${UNIX_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${UNIX_CXX_FLAGS}")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set(CMAKE_C_COMPILER "clang")
    set(CMAKE_C_FLAGS "${CLANG_C_FLAGS}")
    set(CMAKE_CXX_COMPILER "clang++")
    set(CMAKE_CXX_FLAGS "${CLANG_CXX_FLAGS}")
elseif("$ENV{CXX}" MATCHES "mgcc")
    set(CMAKE_C_FLAGS "${UNIX_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${UNIX_CXX_FLAGS}")
    set(CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIBRARIES} -lstdc++")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    set(CMAKE_C_FLAGS "${GNU_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${GNU_CXX_FLAGS}")
else()
    set(CMAKE_C_FLAGS "${UNIX_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${UNIX_CXX_FLAGS}")
endif()

add_executable(palindrome lib/main.cpp lib/palindrome.cpp)

enable_testing()

add_test(NAME palindrometest COMMAND palindrome -t)

add_custom_target(cppcheck COMMAND cppcheck -q --enable=all --inline-suppr --suppressions-list=suppressions.cfg --error-exitcode=1 lib)
add_custom_target(cpplint COMMAND sh recur-cpplint.sh)
add_custom_target(vera++ COMMAND sh recur-vera++.sh)

if(
    CMAKE_SYSTEM_NAME MATCHES "Haiku" OR
    CMAKE_SYSTEM_NAME MATCHES "Minix" OR
    CMAKE_SYSTEM_NAME MATCHES "MirBSD" OR
    CMAKE_SYSTEM_NAME MATCHES "SunOS" OR
    "${CMAKE_GNU_C_MACHINE}" MATCHES "uclibc"
)
    add_custom_target(lint)
elseif(
    MSVC OR
    CMAKE_SYSTEM_NAME MATCHES "OpenBSD" OR
    LSB_RELEASE MATCHES "Void" OR
    CMAKE_SYSTEM_VERSION MATCHES "HBSD"
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

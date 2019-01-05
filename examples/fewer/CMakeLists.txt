cmake_minimum_required(VERSION 3.4)
enable_testing()

if("$ENV{CLOUD_ABI_TRIPLET}" MATCHES "cloudabi")
  set(CMAKE_C_COMPILER "$ENV{CLOUD_ABI_TRIPLET}-cc")
  set(CMAKE_CXX_COMPILER "$ENV{CLOUD_ABI_TRIPLET}-c++")
  set(CMAKE_AR "$ENV{CLOUD_ABI_TRIPLET}-ar" CACHE FILEPATH "Archiver")
  set(CMAKE_NM "$ENV{CLOUD_ABI_TRIPLET}-nm" CACHE FILEPATH "Nm")
  set(CMAKE_RANLIB "$ENV{CLOUD_ABI_TRIPLET}-ranlib" CACHE FILEPATH "Ranlib")
  set(CMAKE_SYSTEM_NAME "Generic")
endif()

project(fewer)

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

if(CMAKE_C_COMPILER_ID MATCHES "clang")
  set(CMAKE_C_FLAGS "-std=c11 -O2 -Wall -Wextra -Wmost -Weverything")
elseif(CMAKE_C_COMPILER_ID MATCHES "gcc")
  set(CMAKE_C_FLAGS "-std=c11 -O2 -Wall -Wextra")
elseif(MSVC)
  set(CMAKE_C_FLAGS "/O2 /W4")
else()
  set(CMAKE_C_FLAGS "-std=c11 -O2 -Wall")
endif()

add_executable(fewer lib/main.c lib/fewer.c)

if("$ENV{CLOUD_ABI_TRIPLET}" MATCHES "cloudabi")
  add_test(NAME fewer_unit_test COMMAND sh -c "cloudabi-run -e bin/fewer <cloudabi-unittest.yml")
  add_test(NAME fewer_integration_test COMMAND sh -c "cloudabi-run -e bin/fewer <cloudabi-test-repl.yml 3<fewer-script.txt")
else()
  add_test(NAME fewer_unit_test COMMAND fewer -t)
  add_test(NAME fewer_integration_test COMMAND sh -c "bin/fewer <fewer-script.txt")
  add_test(NAME fewer_help_test COMMAND fewer -h)
endif()

add_custom_target(cppcheck COMMAND cppcheck -q --enable=all --suppressions-list=suppressions.cfg lib)
add_custom_target(splint COMMAND 	find lib -type f -name '*.c' -print0 | xargs -0 -n1 -r splint)
add_custom_target(vera++ COMMAND find lib -type f -name '*.cpp' -o -name '*.[ch]' -print0 | xargs -0 -n1 -r vera++ -s)

# Missing linter ports
if(MSVC OR
  CMAKE_SYSTEM_NAME MATCHES "SunOS" OR
  CMAKE_SYSTEM_NAME MATCHES "Haiku" OR
  "${CMAKE_GNU_C_MACHINE}" MATCHES "uclibc"
)
add_custom_target(lint)
elseif(CMAKE_SYSTEM_NAME MATCHES "Minix")
  add_custom_target(lint DEPENDS splint)
elseif(CMAKE_SYSTEM_VERSION MATCHES "HBSD")
  add_custom_target(lint DEPENDS splint vera++)
elseif(CMAKE_SYSTEM_NAME MATCHES "OpenBSD" OR
  LSB_RELEASE MATCHES "Void"
)
add_custom_target(lint DEPENDS cppcheck splint)
else()
  add_custom_target(lint DEPENDS cppcheck splint vera++)
endif()

if ("$ENV{CLOUD_ABI_TRIPLET}" MATCHES "cloudabi")
  add_custom_target(valgrind_test COMMAND valgrind --error-exitcode=1 --leak-check=full cloudabi-run -e bin/fewer <cloudabi-unittest.yml)
  add_custom_target(valgrind_repl COMMAND valgrind --error-exitcode=1 --leak-check=full cloudabi-run -e bin/fewer <cloudabi-test-repl.yml 3<fewer-script.txt)

  add_custom_target(valgrind DEPENDS valgrind_test valgrind_repl)
else()
  add_custom_target(valgrind_test COMMAND valgrind --error-exitcode=1 --leak-check=full bin/fewer -t)
  add_custom_target(valgrind_repl COMMAND valgrind --error-exitcode=1 --leak-check=full bin/fewer <fewer-script.txt)
  add_custom_target(valgrind_usage COMMAND valgrind --error-exitcode=1 --leak-check=full bin/fewer -h)

  add_custom_target(valgrind DEPENDS valgrind_test valgrind_repl valgrind_usage)
endif()

# Broken or missing valgrind
if(MSVC OR
  CMAKE_SYSTEM_NAME MATCHES "DragonFly" OR
  CMAKE_SYSTEM_NAME MATCHES "Haiku" OR
  CMAKE_SYSTEM_VERSION MATCHES "HBSD" OR
  CMAKE_SYSTEM_NAME MATCHES "SunOS" OR
  CMAKE_SYSTEM_NAME MATCHES "Darwin" OR
  CMAKE_SYSTEM_NAME MATCHES "Minix" OR
  CMAKE_SYSTEM_NAME MATCHES "NetBSD" OR
  LSB_RELEASE MATCHES "Void" OR
  CMAKE_SYSTEM_NAME MATCHES "OpenBSD"
)
add_custom_target(leaks)
else()
  add_custom_target(leaks DEPENDS valgrind)
endif()

add_custom_target(clean-cmake COMMAND find . -iwholename '*cmake*' -not -name CMakeLists.txt -delete)

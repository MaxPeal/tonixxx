cmake_minimum_required(VERSION 3.4)

project(potato)

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

if(CMAKE_D_COMPILER_ID MATCHES "gdc")
  set(CMAKE_C_FLAGS "-Wall -Werror -Wdeprecated -Wunknown-pragmas")
else()
  set(CMAKE_C_FLAGS "-w")
endif()

add_executable(potato lib/main.d)

enable_testing()

add_test(NAME potatotest COMMAND potato)

add_custom_target(dscanner COMMAND find lib -type f -name '*.d' -print0 | xargs -0 -n1 -r dub run dscanner -- --styleCheck)

add_custom_target(lint DEPENDS dscanner)

add_custom_target(valgrind_test COMMAND valgrind --error-exitcode=1 --leak-check=full bin/potato)
add_custom_target(valgrind DEPENDS valgrind_usage valgrind_test)

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

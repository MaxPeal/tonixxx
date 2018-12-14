cmake_minimum_required(VERSION 3.4)
project(potato D)
set(EXECUTABLE_OUTPUT_PATH "bin")
set(LSB_RELEASE "")

if(CMAKE_SYSTEM_NAME MATCHES "Linux")
  execute_process(COMMAND lsb_release -a OUTPUT_VARIABLE LSB_RELEASE)
endif()

if(CMAKE_D_COMPILER_ID MATCHES "gdc")
  set(CMAKE_C_FLAGS "-Wall -Werror -Wdeprecated -Wunknown-pragmas")
else()
  set(CMAKE_C_FLAGS "-w")
endif()

add_executable(potato lib/main.d)

enable_testing()

add_test(NAME potatotest COMMAND dub test)

add_custom_target(dscanner COMMAND dub run dscanner -- --styleCheck)
add_custom_target(lint DEPENDS dscanner)

add_custom_target(valgrind_test COMMAND valgrind --error-exitcode=1 --leak-check=full .dub/build/potato)
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

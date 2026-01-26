cmake_minimum_required(VERSION 3.15)

project(
  nixie
  VERSION 0.1.0
  LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(MSVC)
  add_compile_options(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -Wpedantic -Wshadow)
endif()

include_directories(include)

set(SOURCES src/main.cpp src/lexer.cpp src/parser.cpp src/codegen.cpp)

add_executable(${PROJECT_NAME} ${SOURCES})

include(CTest)
enable_testing()

# if(BUILD_TESTING)
include(FetchContent)

FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/85087857ad10bd407cd6ed2f52f7ea9752db621f.zip
  DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

set(gtest_force_shared_crt
    ON
    CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

add_executable(compiler_tests tests/lexer_tests.cpp tests/parser_tests.cpp src/lexer.cpp src/parser.cpp)

target_include_directories(compiler_tests PRIVATE include)
if(MSVC)
  target_compile_options(compiler_tests PRIVATE /WX-)
else()
  target_compile_options(compiler_tests PRIVATE -Wno-error)
endif()
target_link_libraries(compiler_tests PRIVATE GTest::gtest_main)
include(GoogleTest)
gtest_discover_tests(compiler_tests)
# endif()

# 1. (Optional) External Libraries If you install {fmt} via 'sudo apt install
#   libfmt-dev' in WSL2: find_package(fmt REQUIRED)
#   target_link_libraries(${PROJECT_NAME} PRIVATE fmt::fmt)

set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                                 "${CMAKE_BINARY_DIR}/bin")

cmake_minimum_required(VERSION 3.15)

project(
  nixie
  VERSION 0.1.0
  LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(MSVC)
  add_compile_options(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -Wpedantic -Wshadow)
endif()

include_directories(include src/include)

set(SOURCES 
  src/compiler/main.cpp
  src/compiler/lexer.cpp
  src/compiler/parser.cpp
  src/compiler/codegen.cpp
)

add_executable(${PROJECT_NAME} ${SOURCES})

target_include_directories(${PROJECT_NAME} PRIVATE include src/include)

add_executable(nixie-ld src/linker/linker.cpp src/linker/main.cpp)
target_include_directories(nixie-ld PRIVATE include src/compiler)
target_include_directories(nixie-ld INTERFACE include src/include)

# Linker library target for tests (without main)
add_library(nixie-ld-lib STATIC src/linker/linker.cpp)
target_include_directories(nixie-ld-lib PUBLIC include src/compiler src/include)

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

FetchContent_Declare(
  vrEmu6502
  GIT_REPOSITORY https://github.com/visrealm/vrEmu6502.git
  GIT_TAG aae98cb14386d832cb7357c99626520b6590bc24
)

set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(vrEmu6502)

add_executable(compiler_tests
    tests/lexer_tests.cpp
    tests/parser_tests.cpp
    tests/codegen_tests.cpp
    tests/codegen_segments_tests.cpp
    tests/codegen_zeropage_tests.cpp
    tests/codegen_o65_tests.cpp
    tests/codegen_integration_tests.cpp
    src/compiler/lexer.cpp
    src/compiler/parser.cpp
    src/compiler/codegen.cpp
)

target_include_directories(compiler_tests PRIVATE include src src/include src/compiler)
if(MSVC)
  target_compile_options(compiler_tests PRIVATE /WX-)
else()
  target_compile_options(compiler_tests PRIVATE -Wno-error)
endif()
target_link_libraries(compiler_tests PRIVATE GTest::gtest_main)
include(GoogleTest)
gtest_discover_tests(compiler_tests)

# Linker tests
add_executable(linker_tests
    tests/linker_tests.cpp
)
target_include_directories(linker_tests PRIVATE include src src/include src/compiler)
if(MSVC)
  target_compile_options(linker_tests PRIVATE /WX-)
else()
  target_compile_options(linker_tests PRIVATE -Wno-error)
endif()
target_link_libraries(linker_tests PRIVATE GTest::gtest_main nixie-ld-lib)
gtest_discover_tests(linker_tests)

# Runtime emulation tests (vrEmu6502)
add_executable(runtime_tests
    tests/runtime_tests.cpp
    src/compiler/lexer.cpp
    src/compiler/parser.cpp
    src/compiler/codegen.cpp
)
target_include_directories(runtime_tests PRIVATE include src src/include src/compiler ${vremu6502_SOURCE_DIR}/src)
if(MSVC)
  target_compile_options(runtime_tests PRIVATE /WX-)
else()
  target_compile_options(runtime_tests PRIVATE -Wno-error)
endif()
target_link_libraries(runtime_tests PRIVATE GTest::gtest_main nixie-ld-lib vrEmu6502)
gtest_discover_tests(runtime_tests)

# Optional py65 integration tests
find_package(Python3 COMPONENTS Interpreter QUIET)
if(Python3_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/tools/run_py65.py")
  add_test(NAME py65_smoke
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tools/run_py65.py --help
  )
endif()

set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                                 "${CMAKE_BINARY_DIR}/bin")

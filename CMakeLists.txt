cmake_minimum_required(VERSION 3.15)

project(nixie 
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

set(SOURCES
    src/main.cpp
)

add_executable(${PROJECT_NAME} ${SOURCES})

# 7. (Optional) External Libraries 
# If you install {fmt} via 'sudo apt install libfmt-dev' in WSL2:
# find_package(fmt REQUIRED)
# target_link_libraries(${PROJECT_NAME} PRIVATE fmt::fmt)

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)
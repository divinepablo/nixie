@echo off
REM Build script for Windows using CMake

setlocal enabledelayedexpansion

REM Check if CMake is installed
cmake --version >nul 2>&1
if errorlevel 1 (
    echo Error: CMake is not installed or not in PATH
    exit /b 1
)

REM Create build directory if it doesn't exist
if not exist build (
    mkdir build
    echo Created build directory
    echo "*" >> build/.gitignore
    echo Added .gitignore to build directory
)

REM Configure and build
cd build
echo Running CMake configuration...
cmake ..
if errorlevel 1 (
    echo Error: CMake configuration failed
    cd ..
    exit /b 1
)

echo Building project...
cmake --build . --config Release
if errorlevel 1 (
    echo Error: Build failed
    cd ..
    exit /b 1
)

cd ..
echo Build completed successfully

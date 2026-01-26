#!/bin/bash
# Build script for Linux using CMake

set -e  # Exit on any error

# Check if CMake is installed
if ! command -v cmake &> /dev/null; then
    echo "Error: CMake is not installed"
    exit 1
fi

# Create build directory if it doesn't exist
if [ ! -d build ]; then
    mkdir build
    echo "Created build directory"
    echo "*" >> build/.gitignore
    echo "Added .gitignore to build directory"
fi

if [ ! -f "./build/.gitignore" ]; then
    echo "*" >> build/.gitignore
    echo "Added .gitignore to build directory"
fi

# Configure and build
cd build
echo "Running CMake configuration..."
cmake ..

echo "Building project..."
cmake --build . --config Release

cd ..
echo "Build completed successfully"

#!/bin/bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/build"
BIN_DIR="$BUILD_DIR/bin"

# Create build directory if it doesn't exist
mkdir -p "$BUILD_DIR"
mkdir -p "$BIN_DIR"

# Get paths to compiler and linker
COMPILER="../build/bin/nixie"
LINKER="../build/bin/nixie-ld"

# Check if compiler and linker exist
if [ ! -f "$COMPILER" ]; then
    echo "Error: Compiler not found at $COMPILER"
    exit 1
fi
if [ ! -f "$LINKER" ]; then
    echo "Error: Linker not found at $LINKER"
    exit 1
fi

echo "Building examples..."
echo

# Iterate through all .nxe files in this directory
for FILE in "$SCRIPT_DIR"/*.nxe; do
    if [ ! -e "$FILE" ]; then
        continue
    fi
    
    FILENAME=$(basename "$FILE")
    BASENAME="${FILENAME%.*}"
    OBJFILE="$BUILD_DIR/$BASENAME.o65"
    BINFILE="$BIN_DIR/$BASENAME.bin"
    
    echo "[1/2] Compiling $FILENAME..."
    "$COMPILER" "$FILE" -o "$OBJFILE"
    if [ $? -ne 0 ]; then
        echo "Error compiling $FILENAME"
        exit 1
    fi
    
    echo "[2/2] Linking $FILENAME..."
    "$LINKER" "$OBJFILE" -o "$BINFILE"
    if [ $? -ne 0 ]; then
        echo "Error linking $FILENAME"
        exit 1
    fi
    
    echo "Successfully built $BINFILE"
    echo
done

echo "All examples built successfully!"
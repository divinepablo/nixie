# Testing Guide

This document describes how to build, run, and extend the Nixie test suites.

## Building Tests

```bash
mkdir -p build && cd build
cmake ..
cmake --build .
```

This produces three test executables:
- `compiler_tests` — lexer, parser, codegen unit/integration tests
- `linker_tests` — O65 linker tests (parsing, relocation, symbol resolution)
- `runtime_tests` — end-to-end 6502 emulation tests (vrEmu6502)

## Running Tests

Run all tests via CTest:

```bash
ctest --test-dir build --output-on-failure
```

Run a specific test suite:

```bash
./build/runtime_tests          # runtime emulation tests only
./build/compiler_tests         # compiler tests only
./build/linker_tests           # linker tests only
```

Run a specific test by name:

```bash
./build/runtime_tests --gtest_filter="BasicExecutionTest.*"
```

## Runtime Emulation Testing

The `runtime_tests` suite uses [vrEmu6502](https://github.com/visrealm/vrEmu6502) (MIT license) to execute linked 6502 binaries in-process. Tests exercise the full pipeline:

1. Build AST nodes (or hand-craft machine code)
2. Generate O65 object file via `CodegenVisitor`
3. Link with `Linker` to produce a flat binary
4. Load binary into vrEmu6502 (W65C02 model)
5. Execute and assert on registers/memory

### Using sim65

[sim65](https://cc65.github.io/doc/sim65.html) is an external 65(C)02 simulator from the cc65 toolchain. It can run linked binaries directly:

```bash
# Compile and link
./build/bin/nixie example.nxe -o example.o65
./build/nixie-ld example.o65 --text 0x0200 -o example.bin

# Run with sim65
sim65 -c 65c02 example.bin
```

**Notes:**
- sim65 expects the binary to start at `$0200` by default
- `BRK` (opcode `$00`) is treated as program exit
- The exit code is the value of the A register at exit

### Using py65

The `tools/run_py65.py` script provides interactive/scripted testing with [py65](https://pypi.org/project/py65/):

```bash
pip install py65

# Basic execution
python tools/run_py65.py build/test.bin --base 0x8000

# With register assertions (CI-friendly: exits 0 on pass, 1 on fail)
python tools/run_py65.py build/test.bin --base 0x8000 --assert A=0x42,X=0x00

# With memory dump
python tools/run_py65.py build/test.bin --base 0x8000 --mem 0x0200:16
```

## Test Organization

| File | Fixture | Purpose |
|------|---------|---------|
| `tests/test_helpers.hpp` | `CodegenTestBase` | Base fixture with AST factories and codegen accessors |
| `tests/o65_builder.hpp` | `O65Builder` | Shared O65 binary builder for constructing test objects |
| `tests/emu_test_helpers.hpp` | `RuntimeTestBase` | Emulator fixture extending `CodegenTestBase` |
| `tests/runtime_tests.cpp` | Various | End-to-end runtime emulation tests |
| `tests/linker_tests.cpp` | `LinkerTest` | Linker unit tests |
| `tests/codegen_*.cpp` | Various | Code generation tests |
| `tests/lexer_tests.cpp` | — | Lexer unit tests |
| `tests/parser_tests.cpp` | — | Parser unit tests |

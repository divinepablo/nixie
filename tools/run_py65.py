#!/usr/bin/env python3
"""
run_py65.py — Standalone 65C02 binary runner using py65.

Loads a flat binary file into a py65 MPU65C02 instance, sets the reset vector,
and executes until BRK. Prints a register dump and optionally checks assertions.

Usage:
    python tools/run_py65.py program.bin [--base 0x8000] [--reset 0x8200]
                                         [--assert A=0x42,X=0x00]
                                         [--mem 0x0200:16]

Requirements:
    pip install py65
"""

import argparse
import sys


def main():
    parser = argparse.ArgumentParser(
        description="Run a 65C02 flat binary using py65"
    )
    parser.add_argument("binary", nargs="?", help="Path to flat binary file")
    parser.add_argument(
        "--base",
        type=lambda x: int(x, 0),
        default=0x8000,
        help="Load address for the binary (default: 0x8000)",
    )
    parser.add_argument(
        "--assert",
        dest="assertions",
        default=None,
        help="Comma-separated register assertions, e.g. A=0x42,X=0x00",
    )
    parser.add_argument(
        "--mem",
        action="append",
        default=[],
        help="Memory range to dump: ADDR:LEN (hex), e.g. 0x0200:16",
    )
    parser.add_argument(
        "--reset",
        type=lambda x: int(x, 0),
        default=None,
        help="Reset vector address (default: same as --base). Use when your "
             "entry point differs from the load address.",
    )
    parser.add_argument(
        "--max-cycles",
        type=int,
        default=100000,
        help="Maximum instructions to execute (default: 100000)",
    )

    args = parser.parse_args()

    if args.binary is None:
        parser.print_help()
        return 0

    try:
        from py65.devices.mpu65c02 import MPU as MPU65C02
    except ImportError:
        print("Error: py65 is not installed. Run: pip install py65", file=sys.stderr)
        return 1

    # Read binary
    with open(args.binary, "rb") as f:
        data = f.read()

    # Create CPU
    mpu = MPU65C02()

    # Load binary at base address
    base = args.base
    for i, byte in enumerate(data):
        if base + i < 0x10000:
            mpu.memory[base + i] = byte

    # Set reset vector (use --reset if provided, otherwise default to --base)
    reset_addr = args.reset if args.reset is not None else base
    mpu.memory[0xFFFC] = reset_addr & 0xFF
    mpu.memory[0xFFFD] = (reset_addr >> 8) & 0xFF

    # Reset CPU
    mpu.reset()

    # Execute until BRK or max cycles
    cycles = 0
    while cycles < args.max_cycles:
        if mpu.memory[mpu.pc] == 0x00:  # BRK
            break
        mpu.step()
        cycles += 1

    # Print register dump
    print(f"PC=${mpu.pc:04X}  A=${mpu.a:02X}  X=${mpu.x:02X}  Y=${mpu.y:02X}  "
          f"SP=${mpu.sp:02X}  P=${mpu.p:02X}")
    print(f"Cycles: {cycles}")

    # Dump memory ranges
    for mem_spec in args.mem:
        try:
            addr_str, length_str = mem_spec.split(":")
            addr = int(addr_str, 0)
            length = int(length_str, 0)
            values = [mpu.memory[addr + i] for i in range(length)]
            hex_str = " ".join(f"{v:02X}" for v in values)
            print(f"Memory ${addr:04X}: {hex_str}")
        except (ValueError, IndexError) as e:
            print(f"Error parsing memory spec '{mem_spec}': {e}", file=sys.stderr)

    # Check assertions
    if args.assertions:
        reg_map = {
            "A": mpu.a,
            "X": mpu.x,
            "Y": mpu.y,
            "SP": mpu.sp,
            "PC": mpu.pc,
            "P": mpu.p,
        }
        failed = False
        for assertion in args.assertions.split(","):
            if "=" not in assertion:
                print(f"FAIL: Invalid assertion format: expected 'REGISTER=VALUE', got '{assertion.strip()}'", file=sys.stderr)
                failed = True
                continue
            name, expected_str = assertion.strip().split("=", 1)
            name = name.strip().upper()
            expected = int(expected_str.strip(), 0)
            actual = reg_map.get(name)
            if actual is None:
                print(f"FAIL: Unknown register '{name}'", file=sys.stderr)
                failed = True
            elif actual != expected:
                print(
                    f"FAIL: {name} expected 0x{expected:02X}, got 0x{actual:02X}",
                    file=sys.stderr,
                )
                failed = True
            else:
                print(f"PASS: {name} == 0x{expected:02X}")

        if failed:
            return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())

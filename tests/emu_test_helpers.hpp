/**
 * @file emu_test_helpers.hpp
 * @brief Runtime test fixture using vrEmu6502 for 65C02 emulation
 *
 * Provides RuntimeTestBase that extends CodegenTestBase with:
 *   - A vrEmu6502 CPU instance (W65C02 model)
 *   - 64KB memory buffer with read/write callbacks
 *   - Helpers for compiling AST → O65 → linked binary → execution
 *   - Register and memory accessors for assertions
 */

#pragma once

#include "test_helpers.hpp"
#include "o65_builder.hpp"
#include "vrEmu6502.h"
#include <array>
#include <vector>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <cstdio>
#include <string>

// ============================================================================
// GLOBAL MEMORY FOR EMULATOR CALLBACKS
// ============================================================================

namespace emu_test {

/// 64KB flat memory space shared by the emulator callbacks
inline std::array<uint8_t, 0x10000>& getMemory() {
    static std::array<uint8_t, 0x10000> memory{};
    return memory;
}

/// vrEmu6502 memory read callback
inline uint8_t memRead(uint16_t addr, bool /*isDbg*/) {
    return getMemory()[addr];
}

/// vrEmu6502 memory write callback
inline void memWrite(uint16_t addr, uint8_t val) {
    getMemory()[addr] = val;
}

} // namespace emu_test

// ============================================================================
// RUNTIME TEST FIXTURE
// ============================================================================

class RuntimeTestBase : public CodegenTestBase {
protected:
    VrEmu6502* cpu = nullptr;

    void SetUp() override {
        CodegenTestBase::SetUp();
        emu_test::getMemory().fill(0);
        cpu = vrEmu6502New(CPU_W65C02, emu_test::memRead, emu_test::memWrite);
        ASSERT_NE(cpu, nullptr) << "Failed to create vrEmu6502 instance";
    }

    void TearDown() override {
        if (cpu) {
            vrEmu6502Destroy(cpu);
            cpu = nullptr;
        }
    }

    // --- Binary loading ---

    /**
     * @brief Load a flat binary into the emulator memory at the given base address,
     *        and set the reset vector (0xFFFC/0xFFFD) to point to base.
     */
    void loadBinary(const std::vector<uint8_t>& binary, uint16_t base) {
        auto& mem = emu_test::getMemory();
        for (size_t i = 0; i < binary.size() && (base + i) < 0x10000; ++i) {
            mem[base + i] = binary[i];
        }
        // Set reset vector to base address
        mem[0xFFFC] = static_cast<uint8_t>(base & 0xFF);
        mem[0xFFFD] = static_cast<uint8_t>((base >> 8) & 0xFF);
    }

    // --- Execution ---

    /**
     * @brief Reset CPU and run until BRK (opcode 0x00) or maxCycles exceeded.
     */
    void run(int maxCycles = 100000) {
        vrEmu6502Reset(cpu);
        int cycles = 0;
        while (cycles < maxCycles) {
            uint8_t opcode = emu_test::getMemory()[vrEmu6502GetPC(cpu)];
            if (opcode == 0x00) break; // BRK
            vrEmu6502InstCycle(cpu);
            ++cycles;
        }
    }

    /**
     * @brief Step N instructions via vrEmu6502InstCycle.
     */
    void step(int n = 1) {
        for (int i = 0; i < n; ++i) {
            vrEmu6502InstCycle(cpu);
        }
    }

    // --- Register accessors ---

    uint8_t getA() const { return vrEmu6502GetAcc(cpu); }
    uint8_t getX() const { return vrEmu6502GetX(cpu); }
    uint8_t getY() const { return vrEmu6502GetY(cpu); }
    uint16_t getPC() const { return vrEmu6502GetPC(cpu); }
    uint8_t getSP() const { return vrEmu6502GetStackPointer(cpu); }
    uint8_t getStatus() const { return vrEmu6502GetStatus(cpu); }

    // --- Memory accessors ---

    uint8_t readMem(uint16_t addr) const {
        return emu_test::getMemory()[addr];
    }

    uint16_t readMemWord(uint16_t addr) const {
        auto& mem = emu_test::getMemory();
        return static_cast<uint16_t>(mem[addr]) |
               (static_cast<uint16_t>(mem[addr + 1]) << 8);
    }

    // --- Compile & link helpers ---

    /**
     * @brief Build a flat binary from raw opcodes using O65Builder → temp file → Linker.
     */
    std::vector<uint8_t> buildO65FromBytes(const std::vector<uint8_t>& opcodes,
                                           uint16_t textBase = 0x8000) {
        O65Builder b;
        b.set_text(opcodes);
        b.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

        auto bytes = b.serialize();
        std::string tmpfile = "/tmp/nixie_runtime_test.o65";
        {
            std::ofstream out(tmpfile, std::ios::binary);
            out.write(reinterpret_cast<const char*>(bytes.data()),
                      static_cast<std::streamsize>(bytes.size()));
        }

        LinkerConfig cfg = make_config({tmpfile}, textBase);
        Linker linker(cfg);
        linker.load_files();
        auto binary = linker.link();

        std::remove(tmpfile.c_str());
        return binary;
    }

    /**
     * @brief Full pipeline: visit AST nodes → generateO65() → temp file → Linker → flat binary.
     *
     * After linking, loads the text segment at textBase and copies the data
     * segment from ROM (appended after text in the binary) to dataBase in the
     * emulator memory, simulating what a RESET handler would do at boot.
     */
    std::vector<uint8_t> compileAndLink(std::vector<std::unique_ptr<Node>>& nodes,
                                        uint16_t textBase = 0x8000,
                                        uint16_t dataBase = 0x0200) {
        // Visit all nodes
        for (auto& node : nodes) {
            node->accept(visitor);
        }

        // Remember text segment size before generating O65 (for data copy)
        size_t textLen = getTextSegment().size();

        // Generate O65 binary
        auto o65 = visitor.generateO65();

        std::string tmpfile = "/tmp/nixie_runtime_compile_test.o65";
        {
            std::ofstream out(tmpfile, std::ios::binary);
            out.write(reinterpret_cast<const char*>(o65.data()),
                      static_cast<std::streamsize>(o65.size()));
        }

        LinkerConfig cfg = make_config({tmpfile}, textBase, dataBase);
        Linker linker(cfg);
        linker.load_files();
        auto binary = linker.link();

        std::remove(tmpfile.c_str());

        // Load text at textBase
        loadBinary(binary, textBase);

        // Copy the data portion (appended after text in ROM) to dataBase,
        // simulating what the RESET handler's data-copy loop does at boot.
        auto& mem = emu_test::getMemory();
        for (size_t i = textLen; i < binary.size(); ++i) {
            uint32_t dst = static_cast<uint32_t>(dataBase) + (i - textLen);
            if (dst < 0x10000) {
                mem[dst] = binary[i];
            }
        }

        return binary;
    }
};

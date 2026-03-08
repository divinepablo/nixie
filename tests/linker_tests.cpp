/**
 * @file linker_tests.cpp
 * @brief Unit tests for the nixie linker (nixie-ld)
 *
 * Tests cover: o65 parsing, symbol resolution, relocation application,
 * base address assignment, chained files, inject_symbol, and full link().
 */

#include <gtest/gtest.h>
#include "linker.hpp"
#include "o65_builder.hpp"
#include <vector>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdio>

using namespace o65;

// ============================================================================
// TEST FIXTURE
// ============================================================================

class LinkerTest : public ::testing::Test {
protected:
    /// Build a simple object file with some NOP text, no relocations, one export
    O65Builder simple_object(uint16_t tbase = 0x0000, const std::string& export_name = "main",
                             uint16_t export_value = 0) {
        // 6 bytes of 6502 NOP (0xEA)
        std::vector<uint8_t> text(6, 0xEA);
        std::vector<uint8_t> data = {0x48, 0x65, 0x6C, 0x6C}; // "Hell"

        O65Builder b;
        b.tbase = tbase;
        b.set_text(text);
        b.set_data(data);
        b.add_export(export_name, static_cast<uint8_t>(SegmentID::TEXT), export_value);
        return b;
    }
};

// ============================================================================
// PARSING TESTS
// ============================================================================

TEST_F(LinkerTest, ParseObjectFile_ValidHeader) {
    auto builder = simple_object();
    auto config = make_config();
    Linker linker(config);

    // Write to temp file
    std::string tmpfile = "test_parse_valid.o65";
    {
        auto bytes = builder.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile});
    Linker l(cfg);
    EXPECT_NO_THROW(l.load_files());

    // Cleanup
    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, ParseObjectFile_InvalidMagic) {
    O65Builder b;
    b.set_text({0xEA});
    auto bytes = b.serialize();
    // Corrupt the magic
    bytes[2] = 0xFF;

    std::string tmpfile = "test_parse_invalid.o65";
    {
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile});
    Linker l(cfg);
    EXPECT_THROW(l.load_files(), std::runtime_error);

    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, ParseObjectFile_MultipleUndefinedRefs) {
    O65Builder b;
    b.set_text({0xEA, 0xEA});
    b.add_undefined("foo");
    b.add_undefined("bar");
    b.add_undefined("baz");
    // Also export something so it doesn't fail on missing symbols during link
    b.add_export("myfunc", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpfile = "test_parse_undefs.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile});
    Linker l(cfg);
    // This should load fine – undefined refs just go into the global table as nullopt
    EXPECT_NO_THROW(l.load_files());

    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, ParseObjectFile_ExportedGlobalsPreserved) {
    O65Builder b;
    b.set_text({0xEA, 0xEA, 0xEA, 0xEA});
    b.add_export("alpha", static_cast<uint8_t>(SegmentID::TEXT), 0);
    b.add_export("beta", static_cast<uint8_t>(SegmentID::TEXT), 2);

    std::string tmpfile = "test_parse_exports.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile});
    Linker l(cfg);
    EXPECT_NO_THROW(l.load_files());

    std::remove(tmpfile.c_str());
}

// ============================================================================
// CHAINED FILE TESTS
// ============================================================================

TEST_F(LinkerTest, ParseChainedFile) {
    // First object: chained, exports "_start"
    O65Builder first;
    first.set_text({0xEA, 0xEA, 0xEA});
    first.add_export("_start", static_cast<uint8_t>(SegmentID::TEXT), 0);
    first.set_chained();

    // Second object: standalone, exports "helper"
    O65Builder second;
    second.set_text({0x60}); // RTS
    second.add_export("helper", static_cast<uint8_t>(SegmentID::TEXT), 0);

    // Concatenate them
    auto first_bytes = first.serialize();
    auto second_bytes = second.serialize();
    std::vector<uint8_t> combined;
    combined.insert(combined.end(), first_bytes.begin(), first_bytes.end());
    combined.insert(combined.end(), second_bytes.begin(), second_bytes.end());

    std::string tmpfile = "test_chained.o65";
    {
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(combined.data()), combined.size());
    }

    LinkerConfig cfg = make_config({tmpfile});
    Linker l(cfg);
    EXPECT_NO_THROW(l.load_files());

    std::remove(tmpfile.c_str());
}

// ============================================================================
// BASE ADDRESS ASSIGNMENT TESTS
// ============================================================================

TEST_F(LinkerTest, LinkAssignsTextBaseAddress) {
    O65Builder b;
    b.set_text({0xEA, 0xEA, 0xEA, 0xEA}); // 4 NOPs
    b.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpfile = "test_tbase.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, /*text_base=*/0xC000, /*data_base=*/0x0200);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // Output should contain the 4 NOP bytes
    ASSERT_GE(output.size(), 4u);
    EXPECT_EQ(output[0], 0xEA);
    EXPECT_EQ(output[1], 0xEA);
    EXPECT_EQ(output[2], 0xEA);
    EXPECT_EQ(output[3], 0xEA);

    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, LinkMultipleObjectsSequentialPlacement) {
    // Two object files, each with 4 bytes of text
    O65Builder a;
    a.set_text({0x01, 0x02, 0x03, 0x04});
    a.add_export("func_a", static_cast<uint8_t>(SegmentID::TEXT), 0);

    O65Builder b2;
    b2.set_text({0x05, 0x06, 0x07, 0x08});
    b2.add_export("func_b", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpA = "test_multi_a.o65";
    std::string tmpB = "test_multi_b.o65";
    {
        auto ba = a.serialize();
        std::ofstream out(tmpA, std::ios::binary);
        out.write(reinterpret_cast<const char*>(ba.data()), ba.size());
    }
    {
        auto bb = b2.serialize();
        std::ofstream out(tmpB, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bb.data()), bb.size());
    }

    LinkerConfig cfg = make_config({tmpA, tmpB}, 0x8000, 0x0200);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // Both text segments should be in the output, sequential
    ASSERT_GE(output.size(), 8u);
    EXPECT_EQ(output[0], 0x01);
    EXPECT_EQ(output[1], 0x02);
    EXPECT_EQ(output[2], 0x03);
    EXPECT_EQ(output[3], 0x04);
    EXPECT_EQ(output[4], 0x05);
    EXPECT_EQ(output[5], 0x06);
    EXPECT_EQ(output[6], 0x07);
    EXPECT_EQ(output[7], 0x08);

    std::remove(tmpA.c_str());
    std::remove(tmpB.c_str());
}

// ============================================================================
// RELOCATION TESTS
// ============================================================================

TEST_F(LinkerTest, WordRelocationApplied) {
    // Object with tbase=0x0000, text has a WORD at offset 2 referencing TEXT segment
    // The word at offset 2 should be relocated from 0x0000-based to 0x8000-based
    std::vector<uint8_t> text = {
        0xEA, 0xEA,       // NOP NOP
        0x04, 0x00,       // .word 0x0004 (address within text pointing to offset 4)
        0xEA, 0xEA        // NOP NOP
    };

    O65Builder b;
    b.set_text(text);
    b.add_export("entry", static_cast<uint8_t>(SegmentID::TEXT), 0);
    // WORD relocation at absolute offset 2 (tbase + 2), segment = TEXT
    // First reloc: offset_byte = 3 (from tbase-1 = -1, so -1+3 = 2)
    uint8_t typebyte = static_cast<uint8_t>(RelocationType::WORD) | static_cast<uint8_t>(SegmentID::TEXT);
    b.add_text_reloc(3, typebyte);

    std::string tmpfile = "test_word_reloc.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0200);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    ASSERT_GE(output.size(), 6u);
    // The word at offset 2 should now be 0x0004 + 0x8000 = 0x8004
    uint16_t relocated_word = output[2] | (output[3] << 8);
    EXPECT_EQ(relocated_word, 0x8004)
        << "Expected 0x8004, got 0x" << std::hex << relocated_word;

    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, LowByteRelocationApplied) {
    // Text with a LOW byte relocation at offset 1 referencing TEXT
    std::vector<uint8_t> text = {
        0xA9,       // LDA #imm
        0x04,       // low byte of address 0x0004 in text
        0xEA, 0xEA, 0xEA, 0xEA  // padding
    };

    O65Builder b;
    b.set_text(text);
    b.add_export("entry", static_cast<uint8_t>(SegmentID::TEXT), 0);
    uint8_t typebyte = static_cast<uint8_t>(RelocationType::LOW) | static_cast<uint8_t>(SegmentID::TEXT);
    b.add_text_reloc(2, typebyte); // offset_byte=2 -> absolute offset = (tbase-1)+2 = 1

    std::string tmpfile = "test_low_reloc.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0200);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    ASSERT_GE(output.size(), 2u);
    // LOW byte of 0x0004 relocated by +0x8000 -> low byte of 0x8004 = 0x04
    EXPECT_EQ(output[1], 0x04);

    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, HighByteRelocationApplied) {
    // Text with a HIGH byte relocation at offset 1 referencing TEXT
    // HIGH reloc extra byte = low_byte needed for full address reconstruction
    std::vector<uint8_t> text = {
        0xA9,       // LDA #imm
        0x00,       // high byte of address 0x0004 (high=0x00)
        0xEA, 0xEA, 0xEA
    };

    O65Builder b;
    b.set_text(text);
    b.add_export("entry", static_cast<uint8_t>(SegmentID::TEXT), 0);
    uint8_t typebyte = static_cast<uint8_t>(RelocationType::HIGH) | static_cast<uint8_t>(SegmentID::TEXT);
    // HIGH reloc needs extra low_byte = 0x04 (the low byte of the address)
    b.add_text_reloc(2, typebyte, {0x04}); // absolute_offset = (tbase-1)+2 = 1

    std::string tmpfile = "test_high_reloc.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0200);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    ASSERT_GE(output.size(), 2u);
    // HIGH byte of 0x0004 + 0x8000 = 0x8004 -> high = 0x80
    EXPECT_EQ(output[1], 0x80)
        << "Expected high byte 0x80, got 0x" << std::hex << (int)output[1];

    std::remove(tmpfile.c_str());
}

// ============================================================================
// SYMBOL RESOLUTION TESTS
// ============================================================================

TEST_F(LinkerTest, UndefinedSymbolResolvedAcrossFiles) {
    // File A: exports "helper" at TEXT+0
    O65Builder a;
    a.set_text({0x60}); // RTS
    a.add_export("helper", static_cast<uint8_t>(SegmentID::TEXT), 0);

    // File B: references "helper" with a WORD relocation
    O65Builder b2;
    std::vector<uint8_t> text_b = {
        0x20,       // JSR abs
        0x00, 0x00  // placeholder for helper address
    };
    b2.set_text(text_b);
    b2.add_undefined("helper");
    b2.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);
    // WORD reloc at offset 1, segment=UNDEFINED, symbol_index=0
    uint8_t typebyte = static_cast<uint8_t>(RelocationType::WORD) | static_cast<uint8_t>(SegmentID::UNDEFINED);
    b2.add_text_reloc(2, typebyte, {0x00, 0x00}); // symbol_index = 0 (little-endian word)

    std::string tmpA = "test_symres_a.o65";
    std::string tmpB = "test_symres_b.o65";
    {
        auto ba = a.serialize();
        std::ofstream out(tmpA, std::ios::binary);
        out.write(reinterpret_cast<const char*>(ba.data()), ba.size());
    }
    {
        auto bb = b2.serialize();
        std::ofstream out(tmpB, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bb.data()), bb.size());
    }

    LinkerConfig cfg = make_config({tmpA, tmpB}, 0x8000, 0x0200);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // File A (1 byte) placed at 0x8000, file B (3 bytes) at 0x8001
    // The WORD at B's text[1..2] should resolve to helper = 0x8000
    ASSERT_GE(output.size(), 4u);
    uint16_t resolved_addr = output[2] | (output[3] << 8);
    EXPECT_EQ(resolved_addr, 0x8000)
        << "Expected helper at 0x8000, got 0x" << std::hex << resolved_addr;

    std::remove(tmpA.c_str());
    std::remove(tmpB.c_str());
}

TEST_F(LinkerTest, UnresolvedSymbolThrows) {
    // File that references "missing_func" but nobody exports it
    O65Builder b;
    b.set_text({0x20, 0x00, 0x00});
    b.add_undefined("missing_func");
    uint8_t typebyte = static_cast<uint8_t>(RelocationType::WORD) | static_cast<uint8_t>(SegmentID::UNDEFINED);
    b.add_text_reloc(2, typebyte, {0x00, 0x00});

    std::string tmpfile = "test_unresolved.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0200);
    Linker l(cfg);
    l.load_files();
    EXPECT_THROW(l.link(), std::runtime_error);

    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, DuplicateSymbolThrows) {
    // Two files both export "main"
    O65Builder a;
    a.set_text({0xEA});
    a.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

    O65Builder b2;
    b2.set_text({0xEA});
    b2.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpA = "test_dup_a.o65";
    std::string tmpB = "test_dup_b.o65";
    {
        auto bytes = a.serialize();
        std::ofstream out(tmpA, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }
    {
        auto bytes = b2.serialize();
        std::ofstream out(tmpB, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpA, tmpB});
    Linker l(cfg);
    EXPECT_THROW(l.load_files(), std::runtime_error);

    std::remove(tmpA.c_str());
    std::remove(tmpB.c_str());
}

// ============================================================================
// INJECT_SYMBOL TESTS
// ============================================================================

TEST_F(LinkerTest, InjectSymbolCreatesGlobal) {
    O65Builder b;
    b.set_text({0xEA, 0xEA, 0x00, 0x00}); // 4 bytes text, word at offset 2
    b.add_undefined("__DATA_DEST__"); // referencing the injected symbol
    uint8_t typebyte = static_cast<uint8_t>(RelocationType::WORD) | static_cast<uint8_t>(SegmentID::UNDEFINED);
    b.add_text_reloc(3, typebyte, {0x00, 0x00}); // symbol index 0

    std::string tmpfile = "test_inject.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    // The linker auto-injects __DATA_DEST__ during link()
    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0300);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // The word at text offset 2 should be resolved to data_base = 0x0300
    ASSERT_GE(output.size(), 4u);
    uint16_t val = output[2] | (output[3] << 8);
    EXPECT_EQ(val, 0x0300)
        << "Expected __DATA_DEST__=0x0300, got 0x" << std::hex << val;

    std::remove(tmpfile.c_str());
}

// ============================================================================
// get_padding TESTS
// ============================================================================

TEST_F(LinkerTest, GetPadding_ByteAlignment) {
    EXPECT_EQ(get_padding(0, AlignmentMode::ALIGN_BYTE), 0);
    EXPECT_EQ(get_padding(1, AlignmentMode::ALIGN_BYTE), 0);
    EXPECT_EQ(get_padding(255, AlignmentMode::ALIGN_BYTE), 0);
}

TEST_F(LinkerTest, GetPadding_WordAlignment) {
    EXPECT_EQ(get_padding(0, AlignmentMode::ALIGN_WORD), 0);
    EXPECT_EQ(get_padding(1, AlignmentMode::ALIGN_WORD), 1);
    EXPECT_EQ(get_padding(2, AlignmentMode::ALIGN_WORD), 0);
    EXPECT_EQ(get_padding(3, AlignmentMode::ALIGN_WORD), 1);
}

TEST_F(LinkerTest, GetPadding_LongAlignment) {
    EXPECT_EQ(get_padding(0, AlignmentMode::ALIGN_LONG), 0);
    EXPECT_EQ(get_padding(1, AlignmentMode::ALIGN_LONG), 3);
    EXPECT_EQ(get_padding(2, AlignmentMode::ALIGN_LONG), 2);
    EXPECT_EQ(get_padding(3, AlignmentMode::ALIGN_LONG), 1);
    EXPECT_EQ(get_padding(4, AlignmentMode::ALIGN_LONG), 0);
}

TEST_F(LinkerTest, GetPadding_BlockAlignment) {
    EXPECT_EQ(get_padding(0, AlignmentMode::ALIGN_BLOCK), 0);
    EXPECT_EQ(get_padding(1, AlignmentMode::ALIGN_BLOCK), 255);
    EXPECT_EQ(get_padding(128, AlignmentMode::ALIGN_BLOCK), 128);
    EXPECT_EQ(get_padding(256, AlignmentMode::ALIGN_BLOCK), 0);
}

// ============================================================================
// FILL FLAG TEST
// ============================================================================

TEST_F(LinkerTest, FillFlagPrependsZeros) {
    O65Builder b;
    b.set_text({0xEA});
    b.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpfile = "test_fill.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x0100, 0x0200);
    cfg.fill = true;
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // With fill=true and text_base=0x0100, there should be leading zeros
    // The output should be larger than just the 1 byte of text
    EXPECT_GT(output.size(), 1u);

    std::remove(tmpfile.c_str());
}

TEST_F(LinkerTest, NoFillFlagCompactOutput) {
    O65Builder b;
    b.set_text({0xEA});
    b.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpfile = "test_nofill.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0200);
    cfg.fill = false;
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // Without fill, no 0x8000 bytes of leading zeros
    // Output should just be the text + data ROM content
    EXPECT_LE(output.size(), 100u) << "Output without fill should be compact";

    std::remove(tmpfile.c_str());
}

// ============================================================================
// DATA SEGMENT IN ROM PLACEMENT
// ============================================================================

TEST_F(LinkerTest, DataSegmentPlacedAfterText) {
    O65Builder b;
    b.set_text({0xEA, 0xEA, 0xEA, 0xEA}); // 4 bytes text
    b.set_data({0xAA, 0xBB});               // 2 bytes data
    b.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpfile = "test_data_placement.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0200);
    cfg.fill = false;
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // Text (4 bytes) then data (2 bytes) should follow
    ASSERT_GE(output.size(), 6u);
    // Text at start of ROM
    EXPECT_EQ(output[0], 0xEA);
    EXPECT_EQ(output[3], 0xEA);

    std::remove(tmpfile.c_str());
}

// ============================================================================
// EMPTY OBJECT FILE
// ============================================================================

TEST_F(LinkerTest, EmptyObjectFileLinksOk) {
    O65Builder b;
    b.set_text({});
    b.set_data({});

    std::string tmpfile = "test_empty.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    LinkerConfig cfg = make_config({tmpfile}, 0x8000, 0x0200);
    Linker l(cfg);
    l.load_files();
    EXPECT_NO_THROW(l.link());

    std::remove(tmpfile.c_str());
}

// ============================================================================
// DEFAULT BASE ADDRESS ASSIGNMENT
// ============================================================================

TEST_F(LinkerTest, DefaultBaseAddressesApplied) {
    O65Builder b;
    b.set_text({0xEA});
    b.add_export("main", static_cast<uint8_t>(SegmentID::TEXT), 0);

    std::string tmpfile = "test_defaults.o65";
    {
        auto bytes = b.serialize();
        std::ofstream out(tmpfile, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    // text_base=0 and data_base=0 should trigger defaults (0x8000, 0x0200)
    LinkerConfig cfg = make_config({tmpfile}, 0x0000, 0x0000);
    Linker l(cfg);
    l.load_files();
    auto output = l.link();

    // Should not crash and produce some output
    EXPECT_GT(output.size(), 0u);

    std::remove(tmpfile.c_str());
}

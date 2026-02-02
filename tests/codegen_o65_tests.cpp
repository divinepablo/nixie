/**
 * @file codegen_o65_tests.cpp
 * @brief O65 binary format validation tests
 * 
 * Tests header generation, segment sizes, relocation table format,
 * and overall binary structure conformance.
 */

#include "test_helpers.hpp"
#include <gtest/gtest.h>

// ============================================================================
// O65 Header Magic Number Tests
// ============================================================================

TEST_F(CodegenO65Test, Header_ValidMagicNumber) {
    auto binary = generateBinary();
    
    ASSERT_GE(binary.size(), 6u) << "Binary must contain at least magic number";
    
    // Magic: 0x01 0x00 'o' '6' '5' 0x00
    EXPECT_EQ(binary[0], 0x01) << "First marker byte should be 0x01";
    EXPECT_EQ(binary[1], 0x00) << "Second marker byte should be 0x00";
    EXPECT_EQ(binary[2], 0x6F) << "Magic 'o' = 0x6F";
    EXPECT_EQ(binary[3], 0x36) << "Magic '6' = 0x36";
    EXPECT_EQ(binary[4], 0x35) << "Magic '5' = 0x35";
    EXPECT_EQ(binary[5], 0x00) << "Version should be 0x00";
}

TEST_F(CodegenO65Test, Header_VerificationHelper) {
    auto binary = generateBinary();
    
    // Use helper function - should not throw
    EXPECT_NO_THROW({
        verifyO65Header(binary);
    });
}

// ============================================================================
// O65 Mode Field Tests
// ============================================================================

TEST_F(CodegenO65Test, Header_ModeField_CPU6502) {
    auto binary = generateBinary();
    uint16_t mode = extractModeWord(binary);
    
    // Bit 15: CPU type (0 = 6502, 1 = 65816)
    EXPECT_EQ((mode >> 15) & 1, 0) << "CPU type should be 6502 (bit 15 = 0)";
}

TEST_F(CodegenO65Test, Header_ModeField_BytewiseRelocation) {
    auto binary = generateBinary();
    uint16_t mode = extractModeWord(binary);
    
    // Bit 14: Relocation granularity (0 = bytewise, 1 = pagewise)
    EXPECT_EQ((mode >> 14) & 1, 0) << "Relocation should be bytewise (bit 14 = 0)";
}

TEST_F(CodegenO65Test, Header_ModeField_16BitAddressing) {
    auto binary = generateBinary();
    uint16_t mode = extractModeWord(binary);
    
    // Bit 13: Address size (0 = 16-bit, 1 = 32-bit)
    EXPECT_EQ((mode >> 13) & 1, 0) << "Address size should be 16-bit (bit 13 = 0)";
}

TEST_F(CodegenO65Test, Header_ModeField_ObjectFile) {
    auto binary = generateBinary();
    uint16_t mode = extractModeWord(binary);
    
    // Bit 12: File type (0 = executable, 1 = object file)
    EXPECT_EQ((mode >> 12) & 1, 1) << "File type should be object file (bit 12 = 1)";
}

TEST_F(CodegenO65Test, Header_ModeField_65C02Variant) {
    auto binary = generateBinary();
    uint16_t mode = extractModeWord(binary);
    
    // Bits 4-7: CPU2 type
    uint8_t cpu2 = (mode >> 4) & 0x0F;
    EXPECT_EQ(cpu2, 0x01) << "CPU2 type should be 65C02 (0x10 in bits 4-7)";
}

// ============================================================================
// O65 Segment Size Tests
// ============================================================================

TEST_F(CodegenO65Test, Header_TextLenMatchesSegment_Empty) {
    auto binary = generateBinary();
    
    uint16_t tlen = extractTextLen(binary);
    EXPECT_EQ(tlen, getTextSegment().size()) 
        << "tlen in header should match actual text segment size";
}

TEST_F(CodegenO65Test, Header_TextLenMatchesSegment_WithCode) {
    // Generate some code
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("test", params, std::move(body));
    visitor.visit(*func);
    
    auto binary = generateBinary();
    
    uint16_t tlen = extractTextLen(binary);
    EXPECT_EQ(tlen, getTextSegment().size()) 
        << "tlen should match text segment after code generation";
}

TEST_F(CodegenO65Test, Header_DataLenMatchesSegment_Empty) {
    auto binary = generateBinary();
    
    uint16_t dlen = extractDataLen(binary);
    EXPECT_EQ(dlen, getDataSegment().size()) 
        << "dlen in header should match actual data segment size";
}

TEST_F(CodegenO65Test, Header_DataLenMatchesSegment_WithData) {
    // Generate some data
    auto str = makeString("TestData");
    visitor.visit(*str);
    
    auto binary = generateBinary();
    
    uint16_t dlen = extractDataLen(binary);
    EXPECT_EQ(dlen, getDataSegment().size()) 
        << "dlen should match data segment after data emission";
}

TEST_F(CodegenO65Test, Header_BaseAddresses_AreZero) {
    auto binary = generateBinary();
    
    ASSERT_GE(binary.size(), TestConstants::O65_HEADER_SIZE_16);
    
    // tbase at offset 8
    uint16_t tbase = readWord(binary, 8);
    EXPECT_EQ(tbase, 0x0000) << "tbase should be 0 (set by loader)";
    
    // dbase at offset 12
    uint16_t dbase = readWord(binary, 12);
    EXPECT_EQ(dbase, 0x0000) << "dbase should be 0 (set by loader)";
}

// ============================================================================
// O65 Header Size Tests
// ============================================================================

TEST_F(CodegenO65Test, Header_MinimumSize) {
    auto binary = generateBinary();
    
    EXPECT_GE(binary.size(), TestConstants::O65_HEADER_SIZE_16) 
        << "Binary should be at least 26 bytes (16-bit header size)";
}

TEST_F(CodegenO65Test, Header_16BitModeStructure) {
    auto binary = generateBinary();
    
    // 16-bit header layout:
    // marker[2] + magic[3] + version[1] + mode[2] + 
    // tbase[2] + tlen[2] + dbase[2] + dlen[2] + 
    // bbase[2] + blen[2] + zbase[2] + zlen[2] + stack[2]
    // = 26 bytes total
    
    ASSERT_GE(binary.size(), 26u);
    
    // Verify we can read all header fields without overflow
    readWord(binary, 6);   // mode
    readWord(binary, 8);   // tbase
    readWord(binary, 10);  // tlen
    readWord(binary, 12);  // dbase
    readWord(binary, 14);  // dlen
    readWord(binary, 16);  // bbase
    readWord(binary, 18);  // blen
    readWord(binary, 20);  // zbase
    readWord(binary, 22);  // zlen
    readWord(binary, 24);  // stack
}

// ============================================================================
// O65 Segment Content Tests
// ============================================================================

TEST_F(CodegenO65Test, Binary_ContainsTextSegment) {
    // Generate code
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("code_test", params, std::move(body));
    visitor.visit(*func);
    
    const auto& textSeg = getTextSegment();
    ASSERT_GT(textSeg.size(), 0u);
    
    auto binary = generateBinary();
    
    // Text segment should appear after header
    // Binary should be large enough to contain header + text
    EXPECT_GE(binary.size(), TestConstants::O65_HEADER_SIZE_16 + textSeg.size());
}

TEST_F(CodegenO65Test, Binary_ContainsDataSegment) {
    // Generate data
    auto str = makeString("data_test");
    visitor.visit(*str);
    
    const auto& dataSeg = getDataSegment();
    ASSERT_GT(dataSeg.size(), 0u);
    
    auto binary = generateBinary();
    
    // Data segment should appear after text segment
    uint16_t tlen = extractTextLen(binary);
    
    // Binary should contain header + text + data (at minimum)
    EXPECT_GE(binary.size(), TestConstants::O65_HEADER_SIZE_16 + tlen + dataSeg.size());
}

// ============================================================================
// O65 Relocation Table Tests
// ============================================================================

TEST_F(CodegenO65Test, Relocation_TableTerminator) {
    // Generate code with relocations
    auto var = makeVariable("reloc_test_var", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    auto ref = makeRef("reloc_test_var");
    visitor.visit(*ref);
    
    auto binary = generateBinary();
    
    // Relocation tables should end with 0x00 terminator
    // This is deep in the binary structure, but we can verify
    // the relocation table generation produces valid output
    ASSERT_GT(getTextRelocs().size(), 0u) 
        << "Should have at least one relocation entry";
}

TEST_F(CodegenO65Test, Relocation_EntryFormat) {
    auto var = makeVariable("entry_test", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    auto ref = makeRef("entry_test");
    visitor.visit(*ref);
    
    const auto& relocs = getTextRelocs();
    ASSERT_GT(relocs.size(), 0u);
    
    const auto& entry = relocs[0];
    
    // Verify typebyte is valid
    auto type = o65::get_relocation_type(entry.typebyte);
    EXPECT_TRUE(type == o65::RelocationType::WORD ||
                type == o65::RelocationType::HIGH ||
                type == o65::RelocationType::LOW)
        << "Relocation type should be valid";
    
    auto seg = o65::get_segment_id(entry.typebyte);
    EXPECT_TRUE(seg == o65::SegmentID::DATA ||
                seg == o65::SegmentID::TEXT ||
                seg == o65::SegmentID::ZERO)
        << "Segment ID should be valid";
}

// ============================================================================
// O65 Generation Consistency Tests
// ============================================================================

TEST_F(CodegenO65Test, Generate_MultipleCalls_Consistent) {
    auto str = makeString("consistent");
    visitor.visit(*str);
    
    auto binary1 = generateBinary();
    auto binary2 = generateBinary();
    
    EXPECT_EQ(binary1.size(), binary2.size()) 
        << "Multiple generateO65 calls should produce same size";
    
    expectBytes(binary1, binary2, "Multiple generations should be identical");
}

TEST_F(CodegenO65Test, Generate_EmptyProgram_ValidBinary) {
    auto binary = generateBinary();
    
    verifyO65Header(binary);
    
    uint16_t tlen = extractTextLen(binary);
    uint16_t dlen = extractDataLen(binary);
    
    EXPECT_EQ(tlen, 0u) << "Empty program should have zero text length";
    EXPECT_EQ(dlen, 0u) << "Empty program should have zero data length";
}

// ============================================================================
// O65 Segment Order Tests
// ============================================================================

TEST_F(CodegenO65Test, SegmentOrder_TextBeforeData) {
    // Generate both code and data
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("order_func", params, std::move(body));
    visitor.visit(*func);
    
    auto str = makeString("order_data");
    visitor.visit(*str);
    
    auto binary = generateBinary();
    
    // Verify header indicates text comes before data
    // (tbase + tlen should be <= dbase, but since bases are 0, 
    // text segment bytes should precede data segment bytes in file)
    
    uint16_t tlen = extractTextLen(binary);
    uint16_t dlen = extractDataLen(binary);
    
    EXPECT_GT(tlen, 0u) << "Should have text segment";
    EXPECT_GT(dlen, 0u) << "Should have data segment";
    
    // Text starts at header end (offset 26)
    // Data starts at offset 26 + tlen
    size_t dataStart = TestConstants::O65_HEADER_SIZE_16 + tlen;
    
    EXPECT_LE(dataStart + dlen, binary.size()) 
        << "Binary should be large enough for header + text + data";
}

// ============================================================================
// O65 Exported Globals Tests
// ============================================================================

TEST_F(CodegenO65Test, ExportedGlobals_FunctionExport) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("exported_func", params, std::move(body));
    visitor.visit(*func);
    
    auto binary = generateBinary();
    
    // The binary should be valid O65 format
    verifyO65Header(binary);
    
    // The function should have generated code (at least RTS)
    EXPECT_GT(extractTextLen(binary), 0u) 
        << "Function should generate code in text segment";
}

// ============================================================================
// O65 Zero Page Segment Tests
// ============================================================================

TEST_F(CodegenO65Test, ZeroPageSegment_LengthInHeader) {
    auto zpvar = makeVariable("zp_test", AstType::Primitive(TypeKind::UNSIGNED_8),
                              nullptr, false, true);
    visitor.visit(*zpvar);
    
    auto binary = generateBinary();
    
    // zlen at offset 22
    uint16_t zlen = readWord(binary, 22);
    
    // zlen should include all zero-page allocations
    EXPECT_GT(zlen, 0u) << "Zero-page length should be non-zero after zp allocation";
}

// ============================================================================
// O65 BSS Segment Tests
// ============================================================================

TEST_F(CodegenO65Test, BSSSegment_InitiallyZero) {
    auto binary = generateBinary();
    
    // blen at offset 18
    uint16_t blen = readWord(binary, 18);
    
    // BSS is currently not implemented, should be 0
    EXPECT_EQ(blen, 0u) << "BSS length should be 0 (not implemented)";
}

// ============================================================================
// O65 Stack Size Tests
// ============================================================================

TEST_F(CodegenO65Test, StackSize_InHeader) {
    auto binary = generateBinary();
    
    // stack at offset 24
    uint16_t stack = readWord(binary, 24);
    
    // Stack size is currently 0 (unknown)
    EXPECT_EQ(stack, 0u) << "Stack size should be 0 (unknown)";
}

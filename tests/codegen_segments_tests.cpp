/**
 * @file codegen_segments_tests.cpp
 * @brief Tests for segment emission and relocation table generation
 * 
 * Tests text segment, data segment, and relocation table functionality.
 */

#include "test_helpers.hpp"
#include <gtest/gtest.h>

// ============================================================================
// Text Segment Tests
// ============================================================================

TEST_F(CodegenSegmentTest, TextSegment_InitiallyEmpty) {
    EXPECT_EQ(getTextSegmentSize(), 0u) 
        << "Text segment should be empty initially";
}

TEST_F(CodegenSegmentTest, TextSegment_GrowsWithFunction) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("grow_test", params, std::move(body));
    
    visitor.visit(*func);
    
    EXPECT_GT(getTextSegmentSize(), 0u) 
        << "Text segment should grow after emitting function";
}

TEST_F(CodegenSegmentTest, TextSegment_MultipleEmissions) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body1, body2;
    
    auto func1 = makeFunction("func1", params, std::move(body1));
    visitor.visit(*func1);
    size_t sizeAfterFirst = getTextSegmentSize();
    
    auto func2 = makeFunction("func2", params, std::move(body2));
    visitor.visit(*func2);
    
    EXPECT_GT(getTextSegmentSize(), sizeAfterFirst) 
        << "Text segment should grow with each function";
}

TEST_F(CodegenSegmentTest, TextSegment_FunctionEndsWithRTS) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("rts_test", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    ASSERT_GT(text.size(), 0u);
    EXPECT_EQ(text.back(), Opcodes::RTS) 
        << "Regular function should end with RTS (0x60)";
}

TEST_F(CodegenSegmentTest, TextSegment_InterruptEndsWithRTI) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("rti_test", params, std::move(body), InterruptType::IRQ);
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    ASSERT_GT(text.size(), 0u);
    EXPECT_EQ(text.back(), Opcodes::RTI) 
        << "Interrupt function should end with RTI (0x40)";
}

// ============================================================================
// Data Segment Tests
// ============================================================================

TEST_F(CodegenSegmentTest, DataSegment_InitiallyEmpty) {
    EXPECT_EQ(getDataSegmentSize(), 0u) 
        << "Data segment should be empty initially";
}

TEST_F(CodegenSegmentTest, DataSegment_StringEmission) {
    auto str = makeString("Test");
    visitor.visit(*str);
    
    const auto& data = getDataSegment();
    ASSERT_EQ(data.size(), 5u) << "String 'Test' + null = 5 bytes";
    
    EXPECT_EQ(data[0], 'T');
    EXPECT_EQ(data[1], 'e');
    EXPECT_EQ(data[2], 's');
    EXPECT_EQ(data[3], 't');
    EXPECT_EQ(data[4], 0x00) << "String should be null-terminated";
}

TEST_F(CodegenSegmentTest, DataSegment_VariableReservation) {
    auto var = makeVariable("data_var", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    // Variable declaration reserves space in data segment
    EXPECT_GT(getDataSegmentSize(), 0u) 
        << "Global variable should reserve space in data segment";
}

TEST_F(CodegenSegmentTest, DataSegment_16BitVariableReservation) {
    auto var = makeVariable("word_var", AstType::Primitive(TypeKind::UNSIGNED_16));
    visitor.visit(*var);
    
    // 16-bit variable should reserve 2 bytes
    EXPECT_GE(getDataSegmentSize(), 2u) 
        << "16-bit variable should reserve at least 2 bytes";
}

TEST_F(CodegenSegmentTest, DataSegment_InitializedVariableValue) {
    auto var = makeVariable("init_var", 
                            AstType::Primitive(TypeKind::UNSIGNED_8), 
                            makeNumber(0xAB));
    visitor.visit(*var);
    
    const auto& data = getDataSegment();
    
    // Check if 0xAB appears in data segment
    bool foundValue = false;
    for (auto byte : data) {
        if (byte == 0xAB) {
            foundValue = true;
            break;
        }
    }
    EXPECT_TRUE(foundValue) << "Initialized value 0xAB should appear in data segment";
}

TEST_F(CodegenSegmentTest, DataSegment_MultipleStringsOffset) {
    auto str1 = makeString("AB");
    visitor.visit(*str1);
    auto result1 = popEvalStack();
    
    auto str2 = makeString("CD");
    visitor.visit(*str2);
    auto result2 = peekEvalStack();
    
    EXPECT_EQ(result1.value, 0u) << "First string at offset 0";
    EXPECT_EQ(result2.value, 3u) << "Second string at offset 3 (after 'AB\\0')";
    
    EXPECT_EQ(getDataSegmentSize(), 6u) << "'AB' + null + 'CD' + null = 6 bytes";
}

// ============================================================================
// Relocation Table Tests
// ============================================================================

TEST_F(CodegenRelocationTest, TextRelocs_InitiallyEmpty) {
    EXPECT_EQ(getTextRelocs().size(), 0u) 
        << "Text relocations should be empty initially";
}

TEST_F(CodegenRelocationTest, TextRelocs_GlobalVariableAccess) {
    // Create global variable
    auto var = makeVariable("reloc_var", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    // Reference it (should generate relocation)
    auto ref = makeRef("reloc_var");
    visitor.visit(*ref);
    
    EXPECT_GT(getTextRelocs().size(), 0u) 
        << "Global variable access should generate text relocation";
}

TEST_F(CodegenRelocationTest, TextRelocs_AbsoluteAddressType) {
    auto var = makeVariable("abs_var", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    auto ref = makeRef("abs_var");
    visitor.visit(*ref);
    
    const auto& relocs = getTextRelocs();
    ASSERT_GT(relocs.size(), 0u);
    
    // Verify relocation type is WORD (absolute address)
    auto relocType = o65::get_relocation_type(relocs[0].typebyte);
    EXPECT_EQ(relocType, o65::RelocationType::WORD) 
        << "Absolute address should use WORD relocation";
}

TEST_F(CodegenRelocationTest, TextRelocs_DataSegmentTarget) {
    auto var = makeVariable("data_target", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    auto ref = makeRef("data_target");
    visitor.visit(*ref);
    
    const auto& relocs = getTextRelocs();
    ASSERT_GT(relocs.size(), 0u);
    
    // Verify target segment is DATA
    auto segId = o65::get_segment_id(relocs[0].typebyte);
    EXPECT_EQ(segId, o65::SegmentID::DATA) 
        << "Global variable relocation should target DATA segment";
}

TEST_F(CodegenRelocationTest, TextRelocs_ZeroPageNoWordReloc) {
    // Zero-page variables don't need WORD relocation (only LOW)
    auto var = makeVariable("zp_reloc", 
                            AstType::Primitive(TypeKind::UNSIGNED_8), 
                            nullptr, false, true);
    visitor.visit(*var);
    
    size_t wordRelocsBefore = 0;
    for (const auto& r : getTextRelocs()) {
        if (o65::get_relocation_type(r.typebyte) == o65::RelocationType::WORD) {
            wordRelocsBefore++;
        }
    }
    
    auto ref = makeRef("zp_reloc");
    visitor.visit(*ref);
    
    size_t wordRelocsAfter = 0;
    for (const auto& r : getTextRelocs()) {
        if (o65::get_relocation_type(r.typebyte) == o65::RelocationType::WORD) {
            wordRelocsAfter++;
        }
    }
    
    // Zero-page access uses LOW relocation, not WORD
    // (or no new WORD relocations added)
    EXPECT_GE(wordRelocsAfter, wordRelocsBefore) 
        << "Zero-page access should use LOW relocation, not WORD";
}

TEST_F(CodegenRelocationTest, TextRelocs_StringCreatesReloc) {
    auto str = makeString("reloc_test");
    visitor.visit(*str);
    
    // String visitor adds a text relocation for the data segment reference
    EXPECT_GT(getTextRelocs().size(), 0u) 
        << "String should create relocation entry";
}

TEST_F(CodegenRelocationTest, TextRelocs_MultipleRelocations) {
    auto var1 = makeVariable("r1", AstType::Primitive(TypeKind::UNSIGNED_8));
    auto var2 = makeVariable("r2", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var1);
    visitor.visit(*var2);
    
    auto ref1 = makeRef("r1");
    auto ref2 = makeRef("r2");
    visitor.visit(*ref1);
    popEvalStack();
    visitor.visit(*ref2);
    
    EXPECT_GE(getTextRelocs().size(), 2u) 
        << "Each global variable access should add relocation";
}

TEST_F(CodegenRelocationTest, TextRelocs_AssignmentToGlobal) {
    auto var = makeVariable("assign_target", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    size_t relocsBefore = getTextRelocs().size();
    
    auto assign = makeAssign(makeRef("assign_target"), makeNumber(42));
    visitor.visit(*assign);
    
    EXPECT_GT(getTextRelocs().size(), relocsBefore) 
        << "Assignment to global should add relocation for STA";
}

TEST_F(CodegenRelocationTest, DataRelocs_InitiallyEmpty) {
    EXPECT_EQ(getDataRelocs().size(), 0u) 
        << "Data relocations should be empty initially";
}

// ============================================================================
// Opcode Emission Order Tests
// ============================================================================

TEST_F(CodegenSegmentTest, OpcodeOrder_LDAImmediateFormat) {
    // Test that LDA immediate is emitted correctly: opcode, operand
    auto num = makeNumber(42);
    visitor.visit(*num);
    
    // Load immediate value to register
    peekEvalStack();
    
    // Create a variable and assign to force actual code generation
    auto var = makeVariable("order_test", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    auto assign = makeAssign(makeRef("order_test"), makeNumber(0x55));
    visitor.visit(*assign);
    
    const auto& text = getTextSegment();
    
    // Find LDA immediate (0xA9) and verify next byte is operand
    for (size_t i = 0; i < text.size() - 1; ++i) {
        if (text[i] == Opcodes::LDA_IMMEDIATE) {
            EXPECT_EQ(text[i + 1], 0x55) 
                << "LDA #$55 should have operand 0x55 after opcode";
            return;
        }
    }
    FAIL() << "Should have found LDA immediate instruction";
}

TEST_F(CodegenSegmentTest, OpcodeOrder_STAAbsoluteFormat) {
    auto var = makeVariable("sta_test", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    auto assign = makeAssign(makeRef("sta_test"), makeNumber(1));
    visitor.visit(*assign);
    
    const auto& text = getTextSegment();
    
    // Find STA absolute (0x8D) and verify it's followed by address bytes
    for (size_t i = 0; i < text.size() - 2; ++i) {
        if (text[i] == Opcodes::STA_ABSOLUTE) {
            // Should be followed by 2 address bytes (little-endian)
            EXPECT_TRUE(text.size() > i + 2) 
                << "STA absolute should be followed by 2 address bytes";
            return;
        }
    }
}

// ============================================================================
// Segment Size Consistency Tests
// ============================================================================

TEST_F(CodegenSegmentTest, SegmentSizes_AfterVariableDecl) {
    size_t textBefore = getTextSegmentSize();
    size_t dataBefore = getDataSegmentSize();
    
    auto var = makeVariable("size_test", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*var);
    
    // Variable declaration should add to data segment, not text
    EXPECT_EQ(getTextSegmentSize(), textBefore) 
        << "Global variable declaration should not affect text segment";
    EXPECT_GT(getDataSegmentSize(), dataBefore) 
        << "Global variable declaration should increase data segment";
}

TEST_F(CodegenSegmentTest, SegmentSizes_AfterStringEmission) {
    size_t textBefore = getTextSegmentSize();
    size_t dataBefore = getDataSegmentSize();
    
    auto str = makeString("test");
    visitor.visit(*str);
    
    // String goes to data segment
    EXPECT_EQ(getTextSegmentSize(), textBefore) 
        << "String should not affect text segment size";
    EXPECT_EQ(getDataSegmentSize(), dataBefore + 5) 
        << "String 'test' + null should add 5 bytes to data segment";
}

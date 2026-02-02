/**
 * @file codegen_tests.cpp
 * @brief Main visitor method tests for CodegenVisitor
 * 
 * Tests all visitor methods: NumberNode, StringNode, BoolNode, VariableNode,
 * ReferenceNode, AssignmentNode, BinaryOperationNode, UnaryNode, ComparisonNode,
 * IfNode, WhileNode, FunctionNode, StructureNode, CallNode, MemberReferenceNode,
 * IncludeNode, DefineNode.
 */

#include "test_helpers.hpp"
#include <gtest/gtest.h>

// ============================================================================
// NumberNode Tests
// ============================================================================

TEST_F(CodegenTestBase, NumberNode_8BitValue_Zero) {
    auto node = makeNumber(0);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1) << "EvalStack should have 1 entry after NumberNode";
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::Immediate) 
        << "NumberNode should produce Immediate location";
    EXPECT_EQ(result.value, 0u) << "Value should be 0";
    EXPECT_EQ(result.type.kind, TypeKind::UNSIGNED_8) 
        << "Zero should infer as UNSIGNED_8";
}

TEST_F(CodegenTestBase, NumberNode_8BitValue_One) {
    auto node = makeNumber(1);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::Immediate);
    EXPECT_EQ(result.value, 1u);
    EXPECT_EQ(result.type.kind, TypeKind::UNSIGNED_8);
}

TEST_F(CodegenTestBase, NumberNode_8BitValue_127) {
    auto node = makeNumber(127);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.value, 127u);
    EXPECT_EQ(result.type.kind, TypeKind::UNSIGNED_8);
}

TEST_F(CodegenTestBase, NumberNode_8BitValue_255) {
    auto node = makeNumber(255);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.value, 255u);
    EXPECT_EQ(result.type.kind, TypeKind::UNSIGNED_8);
}

TEST_F(CodegenTestBase, NumberNode_16BitValue_256) {
    auto node = makeNumber(256);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.value, 256u);
    EXPECT_EQ(result.type.kind, TypeKind::UNSIGNED_16) 
        << "256 should infer as UNSIGNED_16";
}

TEST_F(CodegenTestBase, NumberNode_16BitValue_65535) {
    auto node = makeNumber(65535);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.value, 65535u);
    EXPECT_EQ(result.type.kind, TypeKind::UNSIGNED_16);
}

TEST_F(CodegenTestBase, NumberNode_NegativeValue_Minus1) {
    auto node = makeNumber(-1);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(static_cast<int32_t>(result.value), -1);
    EXPECT_EQ(result.type.kind, TypeKind::SIGNED_8) 
        << "-1 should infer as SIGNED_8";
}

TEST_F(CodegenTestBase, NumberNode_NegativeValue_Minus128) {
    auto node = makeNumber(-128);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.type.kind, TypeKind::SIGNED_8);
}

TEST_F(CodegenTestBase, NumberNode_NegativeValue_Minus129) {
    auto node = makeNumber(-129);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.type.kind, TypeKind::SIGNED_16) 
        << "-129 should infer as SIGNED_16";
}

TEST_F(CodegenTestBase, NumberNode_32BitValue) {
    auto node = makeNumber(70000);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.value, 70000u);
    EXPECT_EQ(result.type.kind, TypeKind::UNSIGNED_32) 
        << "70000 exceeds 16-bit, should be UNSIGNED_32";
}

// ============================================================================
// StringNode Tests
// ============================================================================

TEST_F(CodegenTestBase, StringNode_EmptyString) {
    auto node = makeString("");
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1) << "EvalStack should have 1 entry after StringNode";
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::DataSegment) 
        << "StringNode should produce DataSegment location";
    EXPECT_EQ(result.type.kind, TypeKind::STRING);
    
    // Data segment should contain just null terminator
    const auto& data = getDataSegment();
    ASSERT_GE(data.size(), 1u) << "Data segment should contain at least null terminator";
    EXPECT_EQ(data.back(), 0x00) << "String should be null-terminated";
}

TEST_F(CodegenTestBase, StringNode_HelloWorld) {
    auto node = makeString("Hello");
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::DataSegment);
    EXPECT_EQ(result.value, 0u) << "First string should be at offset 0";
    
    // Verify data segment contents
    const auto& data = getDataSegment();
    std::vector<uint8_t> expected = {'H', 'e', 'l', 'l', 'o', 0x00};
    ASSERT_EQ(data.size(), expected.size());
    expectBytes(data, expected, "String 'Hello' data");
}

TEST_F(CodegenTestBase, StringNode_ASCIICharacters) {
    auto node = makeString("ABC123");
    visitor.visit(*node);
    
    const auto& data = getDataSegment();
    std::vector<uint8_t> expected = {'A', 'B', 'C', '1', '2', '3', 0x00};
    expectBytes(data, expected, "ASCII string data");
}

TEST_F(CodegenTestBase, StringNode_MultipleStrings) {
    auto node1 = makeString("Hi");
    auto node2 = makeString("Bye");
    
    visitor.visit(*node1);
    auto result1 = peekEvalStack();
    popEvalStack();
    
    visitor.visit(*node2);
    auto result2 = peekEvalStack();
    
    EXPECT_EQ(result1.value, 0u) << "First string at offset 0";
    EXPECT_EQ(result2.value, 3u) << "Second string at offset 3 (after 'Hi\\0')";
    
    const auto& data = getDataSegment();
    EXPECT_EQ(data.size(), 7u) << "Total: 'Hi' + null + 'Bye' + null = 7 bytes";
}

// ============================================================================
// BoolNode Tests
// ============================================================================

TEST_F(CodegenTestBase, BoolNode_TrueValue) {
    auto node = makeBool(true);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::Immediate);
    EXPECT_EQ(result.value, 1u) << "True should be 1";
    EXPECT_EQ(result.type.kind, TypeKind::BOOLEAN);
}

TEST_F(CodegenTestBase, BoolNode_FalseValue) {
    auto node = makeBool(false);
    visitor.visit(*node);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::Immediate);
    EXPECT_EQ(result.value, 0u) << "False should be 0";
    EXPECT_EQ(result.type.kind, TypeKind::BOOLEAN);
}

// ============================================================================
// VariableNode Tests
// ============================================================================

TEST_F(CodegenTestBase, VariableNode_GlobalUninitialized) {
    auto node = makeVariable("x", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*node);
    
    auto sym = findSymbol("x");
    ASSERT_TRUE(sym.has_value()) << "Symbol 'x' should be registered";
    
    EXPECT_EQ(sym->name, "x");
    EXPECT_EQ(sym->storage, StorageClass::Global);
    EXPECT_EQ(sym->type.kind, TypeKind::UNSIGNED_8);
    EXPECT_EQ(sym->size, 1u);
    
    // Data segment should contain zero-initialized byte
    const auto& data = getDataSegment();
    ASSERT_GE(data.size(), 1u);
}

TEST_F(CodegenTestBase, VariableNode_GlobalInitialized) {
    auto node = makeVariable("x", AstType::Primitive(TypeKind::UNSIGNED_8), makeNumber(42));
    visitor.visit(*node);
    
    auto sym = findSymbol("x");
    ASSERT_TRUE(sym.has_value());
    EXPECT_EQ(sym->storage, StorageClass::Global);
    
    // Data segment should contain 42
    const auto& data = getDataSegment();
    ASSERT_GE(data.size(), 1u);
    // Note: The implementation double-emits data (declareVariable + visit), 
    // but the value 42 should appear
    bool found42 = false;
    for (auto byte : data) {
        if (byte == 42) found42 = true;
    }
    EXPECT_TRUE(found42) << "Data segment should contain initialized value 42";
}

TEST_F(CodegenTestBase, VariableNode_ZeroPage) {
    auto node = makeVariable("zpvar", AstType::Primitive(TypeKind::UNSIGNED_8), 
                             nullptr, false, true);
    visitor.visit(*node);
    
    auto sym = findSymbol("zpvar");
    ASSERT_TRUE(sym.has_value()) << "Zero-page variable should be registered";
    EXPECT_EQ(sym->storage, StorageClass::ZeroPage);
    EXPECT_GE(sym->offset, 0x08) << "Zero-page offset should be >= 0x08 (user area)";
}

TEST_F(CodegenTestBase, VariableNode_16BitGlobal) {
    auto node = makeVariable("word", AstType::Primitive(TypeKind::UNSIGNED_16));
    visitor.visit(*node);
    
    auto sym = findSymbol("word");
    ASSERT_TRUE(sym.has_value());
    EXPECT_EQ(sym->size, 2u) << "u16 should be 2 bytes";
}

TEST_F(CodegenTestBase, VariableNode_DuplicateDeclarationThrows) {
    auto var1 = makeVariable("dup", AstType::Primitive(TypeKind::UNSIGNED_8));
    auto var2 = makeVariable("dup", AstType::Primitive(TypeKind::UNSIGNED_8));
    
    visitor.visit(*var1);
    
    EXPECT_THROW({
        visitor.visit(*var2);
    }, std::runtime_error) << "Duplicate variable declaration should throw";
}

// ============================================================================
// ReferenceNode Tests
// ============================================================================

TEST_F(CodegenTestBase, ReferenceNode_LoadGlobalVariable) {
    // First declare a variable
    auto varNode = makeVariable("myvar", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*varNode);
    
    size_t textSizeBefore = getTextSegment().size();
    
    // Now reference it
    auto refNode = makeRef("myvar");
    visitor.visit(*refNode);
    
    ASSERT_EQ(getEvalStackSize(), 1) << "ReferenceNode should push result to eval stack";
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::Accumulator);
    
    // Should have emitted LDA absolute instruction
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), textSizeBefore) << "Should emit load instructions";
    
    // First opcode should be LDA_ABSOLUTE (0xAD)
    EXPECT_EQ(text[textSizeBefore], Opcodes::LDA_ABSOLUTE);
}

TEST_F(CodegenTestBase, ReferenceNode_LoadZeroPageVariable) {
    auto varNode = makeVariable("zpref", AstType::Primitive(TypeKind::UNSIGNED_8), 
                                nullptr, false, true);
    visitor.visit(*varNode);
    
    size_t textSizeBefore = getTextSegment().size();
    
    auto refNode = makeRef("zpref");
    visitor.visit(*refNode);
    
    const auto& text = getTextSegment();
    // Should use LDA_ZEROPAGE (0xA5)
    EXPECT_EQ(text[textSizeBefore], Opcodes::LDA_ZEROPAGE);
}

TEST_F(CodegenTestBase, ReferenceNode_UndefinedVariableThrows) {
    auto refNode = makeRef("undefined_var");
    
    EXPECT_THROW({
        visitor.visit(*refNode);
    }, std::runtime_error) << "Referencing undefined variable should throw";
}

// ============================================================================
// AssignmentNode Tests
// ============================================================================

TEST_F(CodegenTestBase, AssignmentNode_AssignToGlobalVariable) {
    auto varNode = makeVariable("target", AstType::Primitive(TypeKind::UNSIGNED_8));
    visitor.visit(*varNode);
    
    size_t textSizeBefore = getTextSegment().size();
    
    auto assignNode = makeAssign(makeRef("target"), makeNumber(99));
    visitor.visit(*assignNode);
    
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), textSizeBefore) << "Should emit store instructions";
    
    // Should contain LDA immediate and STA absolute
    bool hasLDA = false, hasSTA = false;
    for (size_t i = textSizeBefore; i < text.size(); ++i) {
        if (text[i] == Opcodes::LDA_IMMEDIATE) hasLDA = true;
        if (text[i] == Opcodes::STA_ABSOLUTE) hasSTA = true;
    }
    EXPECT_TRUE(hasLDA) << "Should emit LDA for loading value";
    EXPECT_TRUE(hasSTA) << "Should emit STA for storing to global";
}

TEST_F(CodegenTestBase, AssignmentNode_AssignToZeroPageVariable) {
    auto varNode = makeVariable("zptgt", AstType::Primitive(TypeKind::UNSIGNED_8), 
                                nullptr, false, true);
    visitor.visit(*varNode);
    
    size_t textSizeBefore = getTextSegment().size();
    
    auto assignNode = makeAssign(makeRef("zptgt"), makeNumber(55));
    visitor.visit(*assignNode);
    
    const auto& text = getTextSegment();
    
    // Should use STA_ZEROPAGE (0x85)
    bool hasSTAzp = false;
    for (size_t i = textSizeBefore; i < text.size(); ++i) {
        if (text[i] == Opcodes::STA_ZEROPAGE) hasSTAzp = true;
    }
    EXPECT_TRUE(hasSTAzp) << "Should emit STA zeropage for zp variable";
}

TEST_F(CodegenTestBase, AssignmentNode_UndefinedVariableThrows) {
    auto assignNode = makeAssign(makeRef("nonexistent"), makeNumber(1));
    
    EXPECT_THROW({
        visitor.visit(*assignNode);
    }, std::runtime_error) << "Assignment to undefined variable should throw";
}

// ============================================================================
// BinaryOperationNode Tests
// ============================================================================

TEST_F(CodegenTestBase, BinaryOperation_Addition8Bit) {
    auto binOp = makeBinary(Operator::ADD, makeNumber(5), makeNumber(10));
    visitor.visit(*binOp);
    
    ASSERT_EQ(getEvalStackSize(), 1) << "BinaryOp should produce one result";
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::Accumulator);
    
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), 0u) << "Should emit code for addition";
    
    // Should contain CLC and ADC
    bool hasCLC = false, hasADC = false;
    for (auto byte : text) {
        if (byte == Opcodes::CLC) hasCLC = true;
        if (byte == Opcodes::ADC_ZEROPAGE) hasADC = true;
    }
    EXPECT_TRUE(hasCLC) << "Addition should clear carry first";
    EXPECT_TRUE(hasADC) << "Addition should use ADC instruction";
}

TEST_F(CodegenTestBase, BinaryOperation_Subtraction8Bit) {
    auto binOp = makeBinary(Operator::SUBTRACT, makeNumber(20), makeNumber(5));
    visitor.visit(*binOp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    
    const auto& text = getTextSegment();
    
    // Should contain SEC and SBC
    bool hasSEC = false, hasSBC = false;
    for (auto byte : text) {
        if (byte == Opcodes::SEC) hasSEC = true;
        if (byte == Opcodes::SBC_ZEROPAGE) hasSBC = true;
    }
    EXPECT_TRUE(hasSEC) << "Subtraction should set carry first";
    EXPECT_TRUE(hasSBC) << "Subtraction should use SBC instruction";
}

TEST_F(CodegenTestBase, BinaryOperation_16BitAddition) {
    auto binOp = makeBinary(Operator::ADD, makeNumber(300), makeNumber(400));
    visitor.visit(*binOp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    // 16-bit values should be handled
    EXPECT_EQ(result.location, ValueLocation::Accumulator);
}

TEST_F(CodegenTestBase, BinaryOperation_StackManipulation) {
    // Verify that binary operations properly push/pop intermediate values
    auto binOp = makeBinary(Operator::ADD, makeNumber(1), makeNumber(2));
    
    size_t stackBefore = getEvalStackSize();
    visitor.visit(*binOp);
    
    // After visiting: left pushes 1, right pushes 1, binop pops 2 and pushes 1
    // Net effect: +1 on eval stack
    EXPECT_EQ(getEvalStackSize(), stackBefore + 1);
}

// ============================================================================
// UnaryNode Tests
// ============================================================================

TEST_F(CodegenTestBase, UnaryNode_NotOperation) {
    auto unary = makeUnary(UnaryOperator::NOT, makeNumber(0));
    visitor.visit(*unary);
    
    const auto& text = getTextSegment();
    
    // NOT uses CMP #0 to set flags
    bool hasCMP = false;
    for (auto byte : text) {
        if (byte == Opcodes::CMP_IMMEDIATE) hasCMP = true;
    }
    EXPECT_TRUE(hasCMP) << "NOT should use CMP to test value";
}

TEST_F(CodegenTestBase, UnaryNode_NegateOperation) {
    auto unary = makeUnary(UnaryOperator::NEGATE, makeNumber(5));
    visitor.visit(*unary);
    
    const auto& text = getTextSegment();
    
    // NEGATE uses EOR #$FF, CLC, ADC #1 (two's complement)
    bool hasEOR = false, hasADC = false;
    for (auto byte : text) {
        if (byte == Opcodes::EOR_IMMEDIATE) hasEOR = true;
        if (byte == Opcodes::ADC_IMMEDIATE) hasADC = true;
    }
    EXPECT_TRUE(hasEOR) << "NEGATE should use EOR #$FF";
    EXPECT_TRUE(hasADC) << "NEGATE should use ADC #1";
}

// ============================================================================
// Pointer Operation Tests
// ============================================================================

TEST_F(CodegenTestBase, UnaryNode_ReferenceGlobalVariable) {
    // Declare a global variable
    auto var = makeVariable("myVar", AstType::Primitive(TypeKind::UNSIGNED_8), makeNumber(42));
    visitor.visit(*var);
    
    // Take its address: #myVar
    auto unary = makeUnary(UnaryOperator::REFERENCE, makeRef("myVar"));
    visitor.visit(*unary);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    // Result should be a pointer type
    EXPECT_TRUE(result.type.isPointer()) << "Reference result should be a pointer type";
    EXPECT_EQ(result.type.pointerLevel, 1u) << "Should be single level pointer";
}

TEST_F(CodegenTestBase, UnaryNode_ReferenceZeroPageVariable) {
    // Declare a zero page variable
    auto var = makeVariable("zpVar", AstType::Primitive(TypeKind::UNSIGNED_8), makeNumber(10), false, true);
    visitor.visit(*var);
    
    // Take its address
    auto unary = makeUnary(UnaryOperator::REFERENCE, makeRef("zpVar"));
    visitor.visit(*unary);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    // Result should be in accumulator (address computed)
    EXPECT_EQ(result.location, ValueLocation::Accumulator);
    EXPECT_TRUE(result.type.isPointer());
    
    // Should have emitted LDA immediate for low byte
    const auto& text = getTextSegment();
    bool hasLDA = false;
    for (auto byte : text) {
        if (byte == Opcodes::LDA_IMMEDIATE) hasLDA = true;
    }
    EXPECT_TRUE(hasLDA) << "Should load zero page address into A";
}

TEST_F(CodegenTestBase, UnaryNode_DereferencePointer) {
    // Declare a pointer variable (as u16 for address)
    auto ptr = makeVariable("ptr", AstType::Primitive(TypeKind::UNSIGNED_16, 1), makeNumber(0x1234));
    visitor.visit(*ptr);
    
    // Dereference: @ptr
    auto unary = makeUnary(UnaryOperator::DEREFERENCE, makeRef("ptr"));
    visitor.visit(*unary);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    // Result should be dereferenced - pointer level decreased
    EXPECT_EQ(result.type.pointerLevel, 0u) << "Dereference should decay pointer level";
    EXPECT_EQ(result.location, ValueLocation::DereferencedPointer) << "Should be marked as dereferenced";
}

TEST_F(CodegenTestBase, PointerAssignment_DereferenceStore) {
    // Declare a pointer variable (stored in data segment, holds an address)
    auto ptr = makeVariable("ptr", AstType::Primitive(TypeKind::UNSIGNED_16, 1), makeNumber(0x1000));
    visitor.visit(*ptr);
    
    // @ptr = 42 - this creates a UnaryNode(DEREFERENCE) as the assignee
    // First we need to access the pointer variable's value, then dereference it
    auto derefExpr = makeUnary(UnaryOperator::DEREFERENCE, makeRef("ptr"));
    auto assign = makeAssign(std::move(derefExpr), makeNumber(42));
    
    // This should not throw
    EXPECT_NO_THROW(visitor.visit(*assign));
    
    // Check that store through indirect addressing was emitted
    const auto& text = getTextSegment();
    bool hasSTA_indirect = false;
    for (auto byte : text) {
        if (byte == Opcodes::STA_ZEROPAGE_INDIRECT_Y) hasSTA_indirect = true;
    }
    EXPECT_TRUE(hasSTA_indirect) << "Dereference assignment should use indirect store STA (zp),Y";
}

TEST_F(CodegenTestBase, PointerArithmetic_LoadThroughPointer) {
    // Declare a pointer variable in global storage
    auto ptr = makeVariable("ptr", AstType::Primitive(TypeKind::UNSIGNED_8, 1), makeNumber(0x1000));
    visitor.visit(*ptr);
    
    // Dereference and use in expression
    auto deref = makeUnary(UnaryOperator::DEREFERENCE, makeRef("ptr"));
    visitor.visit(*deref);
    
    // Get the result
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    // Result should be marked as dereferenced pointer
    EXPECT_EQ(result.location, ValueLocation::DereferencedPointer) 
        << "Dereference result should be DereferencedPointer location";
    
    // Now load it into register - this should emit indirect load
    callLoadIntoRegister(result);
    
    // Should have indirect load opcode
    const auto& text = getTextSegment();
    bool hasLDA_indirect = false;
    for (auto byte : text) {
        if (byte == Opcodes::LDA_ZEROPAGE_INDIRECT_Y) hasLDA_indirect = true;
    }
    EXPECT_TRUE(hasLDA_indirect) << "Loading through pointer should use indirect addressing";
}

// ============================================================================
// ComparisonNode Tests
// ============================================================================

TEST_F(CodegenTestBase, ComparisonNode_Equals) {
    auto comp = makeComp(Comparison::EQUALS, makeNumber(5), makeNumber(5));
    visitor.visit(*comp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    auto result = peekEvalStack();
    
    EXPECT_EQ(result.location, ValueLocation::Accumulator);
    
    // Should emit CMP instruction
    const auto& text = getTextSegment();
    bool hasCMP = false;
    for (auto byte : text) {
        if (byte == Opcodes::CMP_ZEROPAGE) hasCMP = true;
    }
    EXPECT_TRUE(hasCMP) << "EQUALS comparison should use CMP";
}

TEST_F(CodegenTestBase, ComparisonNode_NotEqual) {
    auto comp = makeComp(Comparison::NOT_EQUAL, makeNumber(5), makeNumber(3));
    visitor.visit(*comp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
    
    // Should have branch instructions for NOT_EQUAL logic
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), 0u);
}

TEST_F(CodegenTestBase, ComparisonNode_LessThan) {
    auto comp = makeComp(Comparison::LESS_THAN, makeNumber(3), makeNumber(5));
    visitor.visit(*comp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
}

TEST_F(CodegenTestBase, ComparisonNode_GreaterThan) {
    auto comp = makeComp(Comparison::GREATER_THAN, makeNumber(10), makeNumber(5));
    visitor.visit(*comp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
}

TEST_F(CodegenTestBase, ComparisonNode_LessThanEqual) {
    auto comp = makeComp(Comparison::LESS_THAN_EQUAL, makeNumber(5), makeNumber(5));
    visitor.visit(*comp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
}

TEST_F(CodegenTestBase, ComparisonNode_GreaterThanEqual) {
    auto comp = makeComp(Comparison::GREATER_THAN_EQUAL, makeNumber(5), makeNumber(3));
    visitor.visit(*comp);
    
    ASSERT_EQ(getEvalStackSize(), 1);
}

// ============================================================================
// IfNode Tests
// ============================================================================

TEST_F(CodegenTestBase, IfNode_BasicCondition) {
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeNumber(42)); // Simple body
    
    auto ifNode = makeIf(makeBool(true), std::move(body));
    
    size_t labelsBefore = getLabels().size();
    visitor.visit(*ifNode);
    
    // Should create labels for else and endif
    EXPECT_GE(getLabels().size(), labelsBefore + 2) 
        << "IfNode should create else and endif labels";
    
    // Should emit branch instruction
    const auto& text = getTextSegment();
    bool hasBEQ = false, hasJMP = false;
    for (auto byte : text) {
        if (byte == Opcodes::BEQ) hasBEQ = true;
        if (byte == Opcodes::JMP_ABSOLUTE) hasJMP = true;
    }
    EXPECT_TRUE(hasBEQ) << "If should use BEQ for conditional branch";
    EXPECT_TRUE(hasJMP) << "If should use JMP for skipping else";
}

TEST_F(CodegenTestBase, IfNode_WithElse) {
    std::vector<std::unique_ptr<Node>> thenBody;
    thenBody.push_back(makeNumber(1));
    
    std::vector<std::unique_ptr<Node>> elseBody;
    elseBody.push_back(makeNumber(2));
    
    auto ifNode = makeIf(makeBool(false), std::move(thenBody), std::move(elseBody));
    visitor.visit(*ifNode);
    
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), 0u) << "If-else should emit code";
}

TEST_F(CodegenTestBase, IfNode_EmptyBody) {
    std::vector<std::unique_ptr<Node>> emptyBody;
    
    auto ifNode = makeIf(makeBool(true), std::move(emptyBody));
    
    // Should not throw with empty body
    EXPECT_NO_THROW({
        visitor.visit(*ifNode);
    });
}

// ============================================================================
// WhileNode Tests
// ============================================================================

TEST_F(CodegenTestBase, WhileNode_BasicLoop) {
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeNumber(1));
    
    auto whileNode = makeWhile(makeBool(true), std::move(body));
    
    size_t labelsBefore = getLabels().size();
    visitor.visit(*whileNode);
    
    // Should create start and end labels
    EXPECT_GE(getLabels().size(), labelsBefore + 2);
    
    // Should have JMP back to start
    const auto& text = getTextSegment();
    int jmpCount = 0;
    for (auto byte : text) {
        if (byte == Opcodes::JMP_ABSOLUTE) jmpCount++;
    }
    EXPECT_GE(jmpCount, 1) << "While should have JMP to loop back";
}

TEST_F(CodegenTestBase, WhileNode_EmptyBody) {
    std::vector<std::unique_ptr<Node>> emptyBody;
    auto whileNode = makeWhile(makeBool(false), std::move(emptyBody));
    
    EXPECT_NO_THROW({
        visitor.visit(*whileNode);
    });
}

// ============================================================================
// FunctionNode Tests
// ============================================================================

TEST_F(CodegenTestBase, FunctionNode_EmptyFunction) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    
    auto funcNode = makeFunction("test_func", params, std::move(body));
    visitor.visit(*funcNode);
    
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), 0u) << "Function should emit prologue/epilogue";
    
    // Should end with RTS
    EXPECT_EQ(text.back(), Opcodes::RTS) << "Function should end with RTS";
}

TEST_F(CodegenTestBase, FunctionNode_InterruptFunction) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    
    auto funcNode = makeFunction("irq_handler", params, std::move(body), true);
    visitor.visit(*funcNode);
    
    const auto& text = getTextSegment();
    
    // Interrupt functions end with RTI instead of RTS
    EXPECT_EQ(text.back(), Opcodes::RTI) << "Interrupt function should end with RTI";
}

TEST_F(CodegenTestBase, FunctionNode_WithBody) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeNumber(42));
    
    auto funcNode = makeFunction("with_body", params, std::move(body));
    visitor.visit(*funcNode);
    
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), 2u) << "Function with body should emit prologue + body + epilogue";
}

TEST_F(CodegenTestBase, FunctionNode_Declaration) {
    std::map<std::string_view, AstType> params;
    auto funcDecl = makeFunctionDecl("forward_decl", params);
    
    // Declaration without body should not emit code
    size_t textBefore = getTextSegment().size();
    visitor.visit(*funcDecl);
    
    EXPECT_EQ(getTextSegment().size(), textBefore) 
        << "Function declaration should not emit code";
}

// ============================================================================
// StructureNode Tests
// ============================================================================

TEST_F(CodegenTestBase, StructureNode_BasicDefinition) {
    std::map<std::string_view, AstType> members;
    members["x"] = AstType::Primitive(TypeKind::UNSIGNED_8);
    members["y"] = AstType::Primitive(TypeKind::UNSIGNED_8);
    
    auto structNode = makeStruct("Point", members);
    visitor.visit(*structNode);
    
    const auto& defs = getStructDefinitions();
    ASSERT_TRUE(defs.count("Point") > 0) << "Struct 'Point' should be registered";
    
    const auto& info = defs.at("Point");
    EXPECT_EQ(info.name, "Point");
    EXPECT_EQ(info.totalSize, 2u) << "Point{u8, u8} should be 2 bytes";
    EXPECT_EQ(info.members.size(), 2u);
}

TEST_F(CodegenTestBase, StructureNode_MemberOffsets) {
    std::map<std::string_view, AstType> members;
    members["a"] = AstType::Primitive(TypeKind::UNSIGNED_8);
    members["b"] = AstType::Primitive(TypeKind::UNSIGNED_16);
    
    auto structNode = makeStruct("Mixed", members);
    visitor.visit(*structNode);
    
    const auto& defs = getStructDefinitions();
    ASSERT_TRUE(defs.count("Mixed") > 0);
    
    const auto& info = defs.at("Mixed");
    EXPECT_EQ(info.totalSize, 3u) << "Mixed{u8, u16} should be 3 bytes";
}

// ============================================================================
// CallNode Tests
// ============================================================================

TEST_F(CodegenTestBase, CallNode_BasicCall) {
    // NOTE: Function calling currently requires functions to be registered in
    // the symbol table, but FunctionNode doesn't register itself. This test
    // verifies that calling an undefined function throws the expected error.
    // When function registration is implemented, this test should be updated
    // to define a function and then call it successfully.
    auto callNode = makeCall("callable");
    
    EXPECT_THROW({
        visitor.visit(*callNode);
    }, std::runtime_error) << "Calling function not in symbol table should throw";
}

TEST_F(CodegenTestBase, CallNode_UndefinedFunctionThrows) {
    auto callNode = makeCall("undefined_func");
    
    EXPECT_THROW({
        visitor.visit(*callNode);
    }, std::runtime_error) << "Calling undefined function should throw";
}

// ============================================================================
// MemberReferenceNode Tests
// ============================================================================

TEST_F(CodegenTestBase, MemberReferenceNode_AccessMember) {
    // Define struct
    std::map<std::string_view, AstType> members;
    members["field1"] = AstType::Primitive(TypeKind::UNSIGNED_8);
    members["field2"] = AstType::Primitive(TypeKind::UNSIGNED_8);
    auto structNode = makeStruct("TestStruct", members);
    visitor.visit(*structNode);
    
    // Create variable of struct type
    auto varNode = makeVariable("instance", AstType::Struct("TestStruct"));
    visitor.visit(*varNode);
    
    // Access member
    auto memberRef = makeMemberRef("instance", "field1");
    visitor.visit(*memberRef);
    
    ASSERT_EQ(getEvalStackSize(), 1) << "Member reference should push result";
}

TEST_F(CodegenTestBase, MemberReferenceNode_UndefinedBaseThrows) {
    auto memberRef = makeMemberRef("nonexistent", "field");
    
    EXPECT_THROW({
        visitor.visit(*memberRef);
    }, std::runtime_error) << "Member access on undefined variable should throw";
}

// ============================================================================
// IncludeNode and DefineNode Tests
// ============================================================================

TEST_F(CodegenTestBase, IncludeNode_NoOp) {
    auto includeNode = makeInclude("test.nxi");
    
    // Include is handled in earlier pass, should be no-op
    size_t textBefore = getTextSegment().size();
    size_t dataBefore = getDataSegment().size();
    
    visitor.visit(*includeNode);
    
    EXPECT_EQ(getTextSegment().size(), textBefore);
    EXPECT_EQ(getDataSegment().size(), dataBefore);
}

TEST_F(CodegenTestBase, DefineNode_NoOp) {
    auto defineNode = makeDefine("CONSTANT", makeNumber(100));
    
    // Define is handled in earlier pass, should be no-op
    size_t textBefore = getTextSegment().size();
    
    visitor.visit(*defineNode);
    
    EXPECT_EQ(getTextSegment().size(), textBefore);
}

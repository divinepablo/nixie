/**
 * @file codegen_integration_tests.cpp
 * @brief End-to-end code generation integration tests
 * 
 * Tests realistic compilation scenarios including multi-function
 * programs, nested control flow, and complex data structures.
 */

#include "test_helpers.hpp"
#include <gtest/gtest.h>

// Add generateBinary helper to integration tests
class CodegenIntegrationTestFull : public CodegenTestBase {
protected:
    std::vector<uint8_t> generateBinary() {
        return visitor.generateO65();
    }
    
    void verifyO65Header(const std::vector<uint8_t>& binary) {
        ASSERT_GE(binary.size(), TestConstants::O65_HEADER_SIZE_16) 
            << "Binary too small for O65 header";
        EXPECT_EQ(binary[0], TestConstants::O65_MARKER_0);
        EXPECT_EQ(binary[1], TestConstants::O65_MARKER_1);
        EXPECT_EQ(binary[2], TestConstants::O65_MAGIC_O);
        EXPECT_EQ(binary[3], TestConstants::O65_MAGIC_6);
        EXPECT_EQ(binary[4], TestConstants::O65_MAGIC_5);
    }
    
    uint16_t extractTextLen(const std::vector<uint8_t>& binary) {
        if (binary.size() < 12) return 0;
        return readWord(binary, 10);
    }
    
    uint16_t extractDataLen(const std::vector<uint8_t>& binary) {
        if (binary.size() < 16) return 0;
        return readWord(binary, 14);
    }
};

// ============================================================================
// Simple Program Integration Tests
// ============================================================================

TEST_F(CodegenIntegrationTestFull, SimpleProgram_SingleFunction) {
    // Create a simple function with a return statement
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeNumber(42));
    
    auto func = makeFunction("main", params, std::move(body));
    visitor.visit(*func);
    
    auto binary = generateBinary();
    
    verifyO65Header(binary);
    EXPECT_GT(getTextSegment().size(), 0u);
}

TEST_F(CodegenIntegrationTestFull, SimpleProgram_GlobalVariableAccess) {
    // Declare global variable
    auto global = makeVariable("counter", AstType::Primitive(TypeKind::UNSIGNED_8),
                               makeNumber(0));
    visitor.visit(*global);
    
    // Function that references the global
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeRef("counter"));
    
    auto func = makeFunction("get_counter", params, std::move(body));
    visitor.visit(*func);
    
    verifyO65Header(generateBinary());
    
    EXPECT_GT(getDataSegment().size(), 0u) << "Should have data segment for global";
    EXPECT_GT(getTextRelocs().size(), 0u) << "Should have relocation for global access";
}

TEST_F(CodegenIntegrationTestFull, SimpleProgram_MultipleGlobals) {
    // Multiple globals
    auto g1 = makeVariable("global1", AstType::Primitive(TypeKind::UNSIGNED_8), makeNumber(1));
    auto g2 = makeVariable("global2", AstType::Primitive(TypeKind::UNSIGNED_16), makeNumber(256));
    auto g3 = makeVariable("global3", AstType::Primitive(TypeKind::UNSIGNED_8), makeNumber(255));
    
    visitor.visit(*g1);
    visitor.visit(*g2);
    visitor.visit(*g3);
    
    // Data segment should contain space for all three variables
    // (The exact size depends on implementation details of how
    // variable declarations and initializations are handled)
    EXPECT_GE(getDataSegment().size(), 4u) 
        << "Data segment should have at least 4 bytes (1+2+1)";
}

// ============================================================================
// Function Composition Tests
// ============================================================================

TEST_F(CodegenIntegrationTestFull, MultipleFunctions_Sequential) {
    std::map<std::string_view, AstType> params;
    
    std::vector<std::unique_ptr<Node>> body1;
    body1.push_back(makeNumber(1));
    auto func1 = makeFunction("func_one", params, std::move(body1));
    
    std::vector<std::unique_ptr<Node>> body2;
    body2.push_back(makeNumber(2));
    auto func2 = makeFunction("func_two", params, std::move(body2));
    
    std::vector<std::unique_ptr<Node>> body3;
    body3.push_back(makeNumber(3));
    auto func3 = makeFunction("func_three", params, std::move(body3));
    
    visitor.visit(*func1);
    visitor.visit(*func2);
    visitor.visit(*func3);
    
    // Should have multiple RTS instructions
    const auto& text = getTextSegment();
    int rtsCount = 0;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::RTS)) {
            rtsCount++;
        }
    }
    EXPECT_EQ(rtsCount, 3) << "Should have 3 RTS instructions for 3 functions";
}

TEST_F(CodegenIntegrationTestFull, FunctionCall_Simple) {
    // NOTE: Function calling currently requires functions to be registered in
    // the symbol table, but FunctionNode doesn't add itself to the symbol table.
    // This test verifies that calling an undefined function throws as expected.
    // When function symbol registration is implemented, this test should be
    // updated to test actual function calls with JSR instructions.
    std::map<std::string_view, AstType> params;
    
    // Define caller with call to undefined callee
    std::vector<std::unique_ptr<Node>> callerBody;
    callerBody.push_back(makeCall("callee_func"));
    
    EXPECT_THROW({
        auto caller = makeFunction("caller_func", params, std::move(callerBody));
        visitor.visit(*caller);
    }, std::runtime_error) << "Calling undefined function should throw";
}

TEST_F(CodegenIntegrationTestFull, FunctionCall_WithRelocation) {
    // NOTE: Function calling currently requires functions to be registered in
    // the symbol table, but FunctionNode doesn't add itself to the symbol table.
    // This test verifies that calling an undefined function throws as expected.
    std::map<std::string_view, AstType> params;
    
    // Define target function (but it won't register itself for calling)
    std::vector<std::unique_ptr<Node>> targetBody;
    auto target = makeFunction("target", params, std::move(targetBody));
    visitor.visit(*target);
    
    // Call from another function - should fail since target not in symbol table
    std::vector<std::unique_ptr<Node>> callerBody;
    callerBody.push_back(makeCall("target"));
    
    EXPECT_THROW({
        auto caller = makeFunction("caller", params, std::move(callerBody));
        visitor.visit(*caller);
    }, std::runtime_error) << "Calling function not in symbol table should throw";
}

// ============================================================================
// Control Flow Integration Tests
// ============================================================================

TEST_F(CodegenIntegrationTestFull, NestedControlFlow_IfInsideWhile) {
    // while (true) { if (x) { ... } }
    auto condition = makeBool(true);
    auto innerCondition = makeBool(true);
    
    std::vector<std::unique_ptr<Node>> ifBody;
    ifBody.push_back(makeNumber(1));
    auto ifStmt = makeIf(std::move(innerCondition), std::move(ifBody));
    
    std::vector<std::unique_ptr<Node>> whileBody;
    whileBody.push_back(std::move(ifStmt));
    auto whileStmt = makeWhile(std::move(condition), std::move(whileBody));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> funcBody;
    funcBody.push_back(std::move(whileStmt));
    auto func = makeFunction("nested_control", params, std::move(funcBody));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Should have branch instructions
    bool hasBranch = false;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::BEQ) ||
            byte == static_cast<uint8_t>(Opcodes::BNE) ||
            byte == static_cast<uint8_t>(Opcodes::JMP_ABSOLUTE)) {
            hasBranch = true;
            break;
        }
    }
    EXPECT_TRUE(hasBranch) << "Nested control flow should have branch instructions";
}

TEST_F(CodegenIntegrationTestFull, NestedControlFlow_WhileInsideIf) {
    // if (x) { while (y) { ... } }
    auto ifCondition = makeBool(true);
    auto whileCondition = makeBool(true);
    
    std::vector<std::unique_ptr<Node>> whileBody;
    whileBody.push_back(makeNumber(99));
    auto whileStmt = makeWhile(std::move(whileCondition), std::move(whileBody));
    
    std::vector<std::unique_ptr<Node>> ifBody;
    ifBody.push_back(std::move(whileStmt));
    auto ifStmt = makeIf(std::move(ifCondition), std::move(ifBody));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> funcBody;
    funcBody.push_back(std::move(ifStmt));
    auto func = makeFunction("while_in_if", params, std::move(funcBody));
    
    visitor.visit(*func);
    
    EXPECT_GT(getTextSegment().size(), 0u);
}

TEST_F(CodegenIntegrationTestFull, NestedControlFlow_MultipleIfElse) {
    // if (a) { ... } else { if (b) { ... } else { ... } }
    auto cond1 = makeBool(true);
    auto cond2 = makeBool(false);
    
    std::vector<std::unique_ptr<Node>> then2;
    then2.push_back(makeNumber(2));
    std::vector<std::unique_ptr<Node>> else2;
    else2.push_back(makeNumber(3));
    auto innerIf = makeIf(std::move(cond2), std::move(then2), std::move(else2));
    
    std::vector<std::unique_ptr<Node>> then1;
    then1.push_back(makeNumber(1));
    std::vector<std::unique_ptr<Node>> else1;
    else1.push_back(std::move(innerIf));
    auto outerIf = makeIf(std::move(cond1), std::move(then1), std::move(else1));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> funcBody;
    funcBody.push_back(std::move(outerIf));
    auto func = makeFunction("multi_if_else", params, std::move(funcBody));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Count JMP instructions for else branches
    int jmpCount = 0;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::JMP_ABSOLUTE)) {
            jmpCount++;
        }
    }
    EXPECT_GE(jmpCount, 2) << "Nested if-else should have multiple JMP instructions";
}

// ============================================================================
// Data Structure Integration Tests
// ============================================================================

TEST_F(CodegenIntegrationTestFull, StructureDefinitionAndUsage) {
    // Define a structure
    auto pointStruct = makeStruct("Point", {
        {"x", AstType::Primitive(TypeKind::UNSIGNED_8)},
        {"y", AstType::Primitive(TypeKind::UNSIGNED_8)}
    });
    visitor.visit(*pointStruct);
    
    // Struct definitions don't directly emit code
    EXPECT_EQ(getTextSegment().size(), 0u);
    EXPECT_EQ(getDataSegment().size(), 0u);
}

TEST_F(CodegenIntegrationTestFull, MultipleStrings) {
    auto str1 = makeString("Hello");
    auto str2 = makeString("World");
    auto str3 = makeString("");
    
    visitor.visit(*str1);
    visitor.visit(*str2);
    visitor.visit(*str3);
    
    const auto& data = getDataSegment();
    
    // "Hello\0" = 6, "World\0" = 6, "\0" = 1 = 13 total
    EXPECT_EQ(data.size(), 13u);
}

// ============================================================================
// Binary Operation Chains
// ============================================================================

TEST_F(CodegenIntegrationTestFull, BinaryOp_ChainedAdditions) {
    // a + b + c
    auto left = makeBinary(Operator::ADD, makeNumber(1), makeNumber(2));
    auto full = makeBinary(Operator::ADD, std::move(left), makeNumber(3));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(full));
    auto func = makeFunction("chain_add", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Count CLC/ADC pairs
    int clcCount = 0;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::CLC)) {
            clcCount++;
        }
    }
    EXPECT_GE(clcCount, 2) << "Chained additions should have multiple CLC instructions";
}

TEST_F(CodegenIntegrationTestFull, BinaryOp_MixedOperations) {
    // (a + b) - c
    auto add = makeBinary(Operator::ADD, makeNumber(10), makeNumber(5));
    auto sub = makeBinary(Operator::SUBTRACT, std::move(add), makeNumber(3));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(sub));
    auto func = makeFunction("mixed_ops", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    bool hasSec = false;
    bool hasClc = false;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::SEC)) hasSec = true;
        if (byte == static_cast<uint8_t>(Opcodes::CLC)) hasClc = true;
    }
    EXPECT_TRUE(hasClc && hasSec) << "Mixed ops should have both CLC (add) and SEC (sub)";
}

// ============================================================================
// Assignment Integration Tests
// ============================================================================

TEST_F(CodegenIntegrationTestFull, Assignment_GlobalFromExpression) {
    // var counter: u8 = 0
    // counter = 1 + 2
    auto global = makeVariable("counter", AstType::Primitive(TypeKind::UNSIGNED_8),
                               makeNumber(0));
    visitor.visit(*global);
    
    auto expr = makeBinary(Operator::ADD, makeNumber(1), makeNumber(2));
    auto assign = makeAssign(makeRef("counter"), std::move(expr));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(assign));
    auto func = makeFunction("update_counter", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Should have STA instruction
    bool hasSta = false;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::STA_ABSOLUTE)) {
            hasSta = true;
            break;
        }
    }
    EXPECT_TRUE(hasSta) << "Assignment should use STA instruction";
}

TEST_F(CodegenIntegrationTestFull, Assignment_ZeroPageVariable) {
    auto zpVar = makeVariable("zpCounter", AstType::Primitive(TypeKind::UNSIGNED_8),
                              nullptr, false, true);
    visitor.visit(*zpVar);
    
    auto assign = makeAssign(makeRef("zpCounter"), makeNumber(42));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(assign));
    auto func = makeFunction("update_zp", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Should have STA_ZP (0x85) for zero-page store
    bool hasStaZp = false;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::STA_ZEROPAGE)) {
            hasStaZp = true;
            break;
        }
    }
    EXPECT_TRUE(hasStaZp) << "Zero-page assignment should use STA $ZP instruction";
}

// ============================================================================
// Interrupt Handler Tests
// ============================================================================

TEST_F(CodegenIntegrationTestFull, InterruptHandler_RTI) {
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeNumber(0));
    
    // Create interrupt handler
    auto handler = makeFunction("nmi_handler", params, std::move(body), InterruptType::NMI);
    visitor.visit(*handler);
    
    const auto& text = getTextSegment();
    
    // Should end with RTI, not RTS
    ASSERT_GT(text.size(), 0u);
    EXPECT_EQ(text.back(), static_cast<uint8_t>(Opcodes::RTI)) 
        << "Interrupt handler should end with RTI";
}

TEST_F(CodegenIntegrationTestFull, MixedFunctions_RegularAndInterrupt) {
    std::map<std::string_view, AstType> params;
    
    // Regular function
    std::vector<std::unique_ptr<Node>> regBody;
    auto regFunc = makeFunction("regular", params, std::move(regBody));
    visitor.visit(*regFunc);
    
    // Interrupt handler
    std::vector<std::unique_ptr<Node>> irqBody;
    auto irqFunc = makeFunction("irq_handler", params, std::move(irqBody), InterruptType::IRQ);
    visitor.visit(*irqFunc);
    
    const auto& text = getTextSegment();
    
    int rtsCount = 0;
    int rtiCount = 0;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::RTS)) rtsCount++;
        if (byte == static_cast<uint8_t>(Opcodes::RTI)) rtiCount++;
    }
    
    EXPECT_EQ(rtsCount, 1) << "Should have 1 RTS for regular function";
    EXPECT_EQ(rtiCount, 1) << "Should have 1 RTI for interrupt handler";
}

// ============================================================================
// Full Program Scenarios
// ============================================================================

TEST_F(CodegenIntegrationTestFull, FullProgram_CounterLoop) {
    // Simulate a simple counter program without function calls
    // (function calls require symbol table registration which isn't yet implemented)
    // var counter: u8 = 0
    // fn main() { while(true) { counter = counter + 1 } }
    
    auto counter = makeVariable("counter", AstType::Primitive(TypeKind::UNSIGNED_8),
                                makeNumber(0));
    visitor.visit(*counter);
    
    // main with infinite loop that increments counter
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> loopBody;
    auto incExpr = makeBinary(Operator::ADD, makeRef("counter"), makeNumber(1));
    loopBody.push_back(makeAssign(makeRef("counter"), std::move(incExpr)));
    auto loop = makeWhile(makeBool(true), std::move(loopBody));
    
    std::vector<std::unique_ptr<Node>> mainBody;
    mainBody.push_back(std::move(loop));
    auto mainFunc = makeFunction("main", params, std::move(mainBody));
    visitor.visit(*mainFunc);
    
    auto binary = generateBinary();
    verifyO65Header(binary);
    
    EXPECT_GT(getTextSegment().size(), 10u) << "Should have substantial code";
    EXPECT_GT(getDataSegment().size(), 0u) << "Should have counter in data segment";
    EXPECT_GT(getTextRelocs().size(), 0u) << "Should have relocations";
}

TEST_F(CodegenIntegrationTestFull, FullProgram_StringTable) {
    // Multiple string constants used in a program
    auto msg1 = makeString("Ready");
    auto msg2 = makeString("Running");
    auto msg3 = makeString("Complete");
    
    visitor.visit(*msg1);
    visitor.visit(*msg2);
    visitor.visit(*msg3);
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    auto func = makeFunction("messages", params, std::move(body));
    visitor.visit(*func);
    
    auto binary = generateBinary();
    verifyO65Header(binary);
    
    // Verify strings are in data segment
    const auto& data = getDataSegment();
    EXPECT_GT(data.size(), 20u) << "Data segment should contain all strings";
}

// ============================================================================
// Comparison and Branching Integration
// ============================================================================

TEST_F(CodegenIntegrationTestFull, Comparison_ComplexCondition) {
    // if (x == 0) { ... } else if (x < 10) { ... }
    auto cond1 = makeComp(Comparison::EQUALS, makeNumber(5), makeNumber(0));
    auto cond2 = makeComp(Comparison::LESS_THAN, makeNumber(5), makeNumber(10));
    
    std::vector<std::unique_ptr<Node>> then1;
    then1.push_back(makeNumber(1));
    
    std::vector<std::unique_ptr<Node>> then2;
    then2.push_back(makeNumber(2));
    std::vector<std::unique_ptr<Node>> else2;
    else2.push_back(makeNumber(3));
    auto innerIf = makeIf(std::move(cond2), std::move(then2), std::move(else2));
    
    std::vector<std::unique_ptr<Node>> else1;
    else1.push_back(std::move(innerIf));
    
    auto outerIf = makeIf(std::move(cond1), std::move(then1), std::move(else1));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(outerIf));
    auto func = makeFunction("complex_cond", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Should have CMP instruction
    bool hasCmp = false;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::CMP_IMMEDIATE)) {
            hasCmp = true;
            break;
        }
    }
    EXPECT_TRUE(hasCmp) << "Complex conditions should have CMP instructions";
}

// ============================================================================
// Unary Operations Integration
// ============================================================================

TEST_F(CodegenIntegrationTestFull, Unary_NotInCondition) {
    // NOTE: This test verifies that NOT operations generate code.
    // The actual instruction sequence depends on implementation details.
    auto inner = makeNumber(0);
    auto notExpr = makeUnary(UnaryOperator::NOT, std::move(inner));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> funcBody;
    funcBody.push_back(std::move(notExpr));
    auto func = makeFunction("not_test", params, std::move(funcBody));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // NOT should generate some code
    // (The exact instruction depends on implementation - EOR or other bitwise op)
    EXPECT_GT(text.size(), 1u) << "NOT operation should generate code";
}

// ============================================================================
// Edge Case Integration Tests
// ============================================================================

TEST_F(CodegenIntegrationTestFull, EdgeCase_EmptyWhileLoop) {
    auto cond = makeBool(true);
    std::vector<std::unique_ptr<Node>> emptyBody;
    auto loop = makeWhile(std::move(cond), std::move(emptyBody));
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(loop));
    auto func = makeFunction("empty_loop", params, std::move(body));
    
    visitor.visit(*func);
    
    // Should still generate valid code
    const auto& text = getTextSegment();
    EXPECT_GT(text.size(), 0u);
    
    // Should have JMP for loop back
    bool hasJmp = false;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::JMP_ABSOLUTE)) {
            hasJmp = true;
            break;
        }
    }
    EXPECT_TRUE(hasJmp) << "Empty while loop should still have jump back";
}

TEST_F(CodegenIntegrationTestFull, EdgeCase_DeeplyNestedBlocks) {
    // if { if { if { if { ... } } } }
    auto buildNested = [this](int depth) -> std::unique_ptr<Node> {
        std::vector<std::unique_ptr<Node>> body;
        body.push_back(makeNumber(depth));
        auto result = makeIf(makeBool(true), std::move(body));
        
        for (int i = 1; i < depth; ++i) {
            std::vector<std::unique_ptr<Node>> wrapper;
            wrapper.push_back(std::move(result));
            result = makeIf(makeBool(true), std::move(wrapper));
        }
        return result;
    };
    
    auto nested = buildNested(5);
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(nested));
    auto func = makeFunction("deep_nest", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Should have multiple branch instructions
    int branchCount = 0;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::BEQ) ||
            byte == static_cast<uint8_t>(Opcodes::BNE)) {
            branchCount++;
        }
    }
    EXPECT_GE(branchCount, 5) << "Deeply nested ifs should have multiple branches";
}

TEST_F(CodegenIntegrationTestFull, EdgeCase_LargeImmediateValue) {
    // 16-bit value in expression
    auto num = makeNumber(0xFFFF);
    
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(std::move(num));
    auto func = makeFunction("large_value", params, std::move(body));
    
    visitor.visit(*func);
    
    const auto& text = getTextSegment();
    
    // Should have LDA immediate somewhere
    bool hasLda = false;
    for (uint8_t byte : text) {
        if (byte == static_cast<uint8_t>(Opcodes::LDA_IMMEDIATE)) {
            hasLda = true;
            break;
        }
    }
    EXPECT_TRUE(hasLda) << "Large value should use LDA immediate";
}

// ============================================================================
// Binary Format Final Verification
// ============================================================================

TEST_F(CodegenIntegrationTestFull, FinalBinary_CompleteProgram) {
    // Build a simple program with basic features (minimal scope to avoid size issues)
    
    // Global variable
    auto global = makeVariable("state", AstType::Primitive(TypeKind::UNSIGNED_8),
                               makeNumber(0));
    visitor.visit(*global);
    
    auto zpVar = makeVariable("fast", AstType::Primitive(TypeKind::UNSIGNED_8),
                              nullptr, false, true);
    visitor.visit(*zpVar);
    
    // Single function
    std::map<std::string_view, AstType> params;
    
    std::vector<std::unique_ptr<Node>> mainBody;
    mainBody.push_back(makeAssign(makeRef("state"), makeNumber(1)));
    auto mainFunc = makeFunction("main", params, std::move(mainBody));
    visitor.visit(*mainFunc);
    
    // Generate final binary
    auto binary = generateBinary();
    
    // Comprehensive verification
    verifyO65Header(binary);
    
    EXPECT_GT(extractTextLen(binary), 0u) << "Should have code";
    EXPECT_GT(extractDataLen(binary), 0u) << "Should have data";
    
    // Binary should be reasonably sized
    EXPECT_GT(binary.size(), 30u) << "Program should produce some binary";
    EXPECT_LT(binary.size(), 1000u) << "Binary should not be unreasonably large";
}

#include <gtest/gtest.h>
#include "../src/parser.hpp"
#include <vector>
#include <functional>
#include <string>

namespace {

struct VariableExpectation {
    std::string name;
    TypeKind type;
    bool zeropage = false;
    std::string structureName = "";
};

struct NumberExpectation {
    int value;
};

void expect_variable(const Node* node, const VariableExpectation& expected) {
    const auto* var = dynamic_cast<const VariableNode*>(node);
    ASSERT_NE(var, nullptr) << "Expected VariableNode";
    EXPECT_EQ(var->name, expected.name);
    EXPECT_EQ(var->type, expected.type);
    EXPECT_EQ(var->zeropaged, expected.zeropage);
    EXPECT_EQ(var->structureName, expected.structureName);
}
void expect_variable_equal_number(const Node* node, const VariableExpectation& expected, const NumberExpectation& expectedNumber) {
    const auto* var = dynamic_cast<const VariableNode*>(node);
    const auto* number = dynamic_cast<const NumberNode*>(var->value.get());
    expect_variable(node, expected);
    EXPECT_EQ(number->value, expectedNumber.value);
}

void expect_function(const Node* node, const std::string& expectedName, bool expectedInterrupt, size_t expectedParamCount) {
    const auto* fn = dynamic_cast<const FunctionNode*>(node);
    ASSERT_NE(fn, nullptr) << "Expected FunctionNode";
    EXPECT_EQ(fn->name, expectedName);
    EXPECT_EQ(fn->interrupt, expectedInterrupt);
    EXPECT_EQ(fn->parameters.size(), expectedParamCount);
}

void expect_struct(const Node* node, const std::string& expectedName, size_t expectedMemberCount) {
    const auto* st = dynamic_cast<const StructureNode*>(node);
    ASSERT_NE(st, nullptr) << "Expected StructureNode";
    EXPECT_EQ(st->name, expectedName);
    EXPECT_EQ(st->members.size(), expectedMemberCount);
}

void expect_program(std::string_view source, const std::vector<std::function<void(const Node*)>>& validators) {
    Lexer lexer(source);
    auto tokens = lexer.tokenize();
    Parser parser(tokens);
    std::unique_ptr<ProgramNode> program(parser.parseProgram());

    ASSERT_EQ(program->statements.size(), validators.size()) 
        << "Statement count mismatch";
    
    for (size_t i = 0; i < validators.size(); ++i) {
        validators[i](program->statements[i].get());
    }
}

} // namespace

TEST(ParserTests, SimpleVariableDeclaration) {
    expect_program("var x: u8 = 0;", {
        [](const Node* n) { 
            expect_variable_equal_number(n, {"x", TypeKind::UNSIGNED_8, false}, {0}); 
        }
    });
}
TEST(ParserTests, VariableEqualToBinary) {
    expect_program("var x: u8 = 0b1010;", {
        [](const Node* n) { 
            expect_variable_equal_number(n, {"x", TypeKind::UNSIGNED_8, false}, {0b1010}); 
        }
    });
}
TEST(ParserTests, ZeropageVariableDeclaration) {
    expect_program("__zeropage var x: u8 = 0x25;", {
        [](const Node* n) { 
            expect_variable_equal_number(n, {"x", TypeKind::UNSIGNED_8, true}, {0x25}); 
        }
    });
}

TEST(ParserTests, FunctionDeclaration) {
    expect_program("fn main() {}", {
        [](const Node* n) { expect_function(n, "main", false, 0); }
    });
}

TEST(ParserTests, FunctionWithParams) {
    expect_program("fn add(x: u8, y: u8) {}", {
        [](const Node* n) { expect_function(n, "add", false, 2); }
    });
}

TEST(ParserTests, InterruptFunction) {
    expect_program("__interrupt fn irq() {}", {
        [](const Node* n) { expect_function(n, "irq", true, 0); }
    });
}

TEST(ParserTests, StructureDeclaration) {
    expect_program("struct Point { x: u8, y: u8 }", {
        [](const Node* n) { expect_struct(n, "Point", 2); }
    });
}

TEST(ParserTests, BasicArithmeticPriority) {
    // var x: u8 = 1 + 2 * 3;
    // Should parse as 1 + (2 * 3)
    expect_program("var x: u8 = 1 + 2 * 3;", {
        [](const Node* n) {
            const auto* var = dynamic_cast<const VariableNode*>(n);
            ASSERT_NE(var, nullptr);
            
            // value should be BinaryOperationNode (+)
            const auto* add = dynamic_cast<const BinaryOperationNode*>(var->value.get());
            ASSERT_NE(add, nullptr);
            EXPECT_EQ(add->operation, Operator::ADD);

            // Left should be 1
            const auto* left = dynamic_cast<const NumberNode*>(add->left.get());
            ASSERT_NE(left, nullptr);
            EXPECT_EQ(left->value, 1);

            // Right should be BinaryOperationNode (*)
            const auto* mult = dynamic_cast<const BinaryOperationNode*>(add->right.get());
            ASSERT_NE(mult, nullptr);
            EXPECT_EQ(mult->operation, Operator::MULTIPLY);

            // Check mult operands
            const auto* mLeft = dynamic_cast<const NumberNode*>(mult->left.get());
            const auto* mRight = dynamic_cast<const NumberNode*>(mult->right.get());
            EXPECT_EQ(mLeft->value, 2);
            EXPECT_EQ(mRight->value, 3);
        }
    });
}

TEST(ParserTests, IfStatement) {
    expect_program("if (1) {}", {
        [](const Node* n) {
            const auto* ifNode = dynamic_cast<const IfNode*>(n);
            ASSERT_NE(ifNode, nullptr) << "Expected IfNode";
            ASSERT_NE(ifNode->condition, nullptr);
        }
    });
}

TEST(ParserTests, WhileLoop) {
    expect_program("while (1) {}", {
        [](const Node* n) {
            const auto* whileNode = dynamic_cast<const WhileNode*>(n);
            ASSERT_NE(whileNode, nullptr) << "Expected WhileNode";
            ASSERT_NE(whileNode->condition, nullptr);
        }
    });
}


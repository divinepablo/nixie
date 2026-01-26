#include "../src/lexer.hpp"

#include <gtest/gtest.h>
#include <string>
#include <vector>

namespace {
struct TokenExpectation {
    Type type;
    std::string value;
};

void expect_tokens(std::string_view source, const std::vector<TokenExpectation>& expected) {
    Lexer lexer(source);
    const auto tokens = lexer.tokenize();

    ASSERT_EQ(tokens.size(), expected.size()) << "Token count mismatch";

    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(tokens[i].type, expected[i].type) << "Token type mismatch at index " << i;
        EXPECT_EQ(tokens[i].value, expected[i].value) << "Token value mismatch at index " << i;
    }
}
} // namespace

TEST(LexerTests, ParsesKeywordsAndSymbols) {
    const std::string source = "fn main() { var x = 123; }";

    expect_tokens(source, {
        {Type::FUNCTION, "fn"},
        {Type::IDENTIFIER, "main"},
        {Type::OPEN_PAREN, "("},
        {Type::CLOSE_PAREN, ")"},
        {Type::OPEN_BRACE, "{"},
        {Type::VARIABLE, "var"},
        {Type::IDENTIFIER, "x"},
        {Type::ASSIGN, "="},
        {Type::NUMBER, "123"},
        {Type::SEMICOLON, ";"},
        {Type::CLOSE_BRACE, "}"},
        {Type::END, ""},
    });
}

TEST(LexerTests, ParsesStringLiteral) {
    const std::string source = "var message = \"hello world\";";

    expect_tokens(source, {
        {Type::VARIABLE, "var"},
        {Type::IDENTIFIER, "message"},
        {Type::ASSIGN, "="},
        {Type::STRING, "hello world"},
        {Type::SEMICOLON, ";"},
        {Type::END, ""},
    });
}

TEST(LexerTests, ParsesNumericFormats) {
    const std::string source = "var hex = 0x2A; var bin = 0b1010; var dec = 42;";

    expect_tokens(source, {
        {Type::VARIABLE, "var"},
        {Type::IDENTIFIER, "hex"},
        {Type::ASSIGN, "="},
        {Type::NUMBER, "0x2A"},
        {Type::SEMICOLON, ";"},

        {Type::VARIABLE, "var"},
        {Type::IDENTIFIER, "bin"},
        {Type::ASSIGN, "="},
        {Type::NUMBER, "0b1010"},
        {Type::SEMICOLON, ";"},

        {Type::VARIABLE, "var"},
        {Type::IDENTIFIER, "dec"},
        {Type::ASSIGN, "="},
        {Type::NUMBER, "42"},
        {Type::SEMICOLON, ";"},
        {Type::END, ""},
    });
}

TEST(LexerTests, SkipsWhitespaceAndComments) {
    const std::string source = "   // comment\nvar a = 1";

    expect_tokens(source, {
        {Type::VARIABLE, "var"},
        {Type::IDENTIFIER, "a"},
        {Type::ASSIGN, "="},
        {Type::NUMBER, "1"},
        {Type::END, ""},
    });
}

TEST(LexerTests, ParsesComparisonOperators) {
    const std::string source = "if (a == b) { return a >= b; }";

    expect_tokens(source, {
        {Type::IF, "if"},
        {Type::OPEN_PAREN, "("},
        {Type::IDENTIFIER, "a"},
        {Type::EQUALS, "=="},
        {Type::IDENTIFIER, "b"},
        {Type::CLOSE_PAREN, ")"},
        {Type::OPEN_BRACE, "{"},
        {Type::IDENTIFIER, "return"},
        {Type::IDENTIFIER, "a"},
        {Type::GREATER_THAN_EQUAL, ">="},
        {Type::IDENTIFIER, "b"},
        {Type::SEMICOLON, ";"},
        {Type::CLOSE_BRACE, "}"},
        {Type::END, ""},
    });
}

TEST(LexerTests, EmptyInput) {
    const std::string source = "";

    expect_tokens(source, {
        {Type::END, ""},
    });
}

TEST(LexerTests, FunctionDeclaration) {
    const std::string source = "fn log_move(x: u8, y: u8);";
    expect_tokens(source, {
        {Type::FUNCTION, "fn"},
        {Type::IDENTIFIER, "log_move"},
        {Type::OPEN_PAREN, "("},
        {Type::IDENTIFIER, "x"},
        {Type::COLON, ":"},
        {Type::TYPE_UNSIGNED_8, "u8"},
        {Type::COMMA, ","},
        {Type::IDENTIFIER, "y"},
        {Type::COLON, ":"},
        {Type::TYPE_UNSIGNED_8, "u8"},
        {Type::CLOSE_PAREN, ")"},
        {Type::SEMICOLON, ";"},
        {Type::END, ""},
    });
}

TEST(LexerTests, WhileLoop) {
    const std::string source = "while (i < 10) { i = i + 1; }";
    expect_tokens(source, {
        {Type::WHILE, "while"},
        {Type::OPEN_PAREN, "("},
        {Type::IDENTIFIER, "i"},
        {Type::LESS_THAN, "<"},
        {Type::NUMBER, "10"},
        {Type::CLOSE_PAREN, ")"},
        {Type::OPEN_BRACE, "{"},
        {Type::IDENTIFIER, "i"},
        {Type::ASSIGN, "="},
        {Type::IDENTIFIER, "i"},
        {Type::PLUS, "+"},
        {Type::NUMBER, "1"},
        {Type::SEMICOLON, ";"},
        {Type::CLOSE_BRACE, "}"},
        {Type::END, ""},
    });
}

TEST(LexerTests, NotEqualOperator) {
    const std::string source = "if (a != b) { do_something(); }";
    expect_tokens(source, {
        {Type::IF, "if"},
        {Type::OPEN_PAREN, "("},
        {Type::IDENTIFIER, "a"},
        {Type::NOT_EQUAL, "!="},
        {Type::IDENTIFIER, "b"},
        {Type::CLOSE_PAREN, ")"},
        {Type::OPEN_BRACE, "{"},
        {Type::IDENTIFIER, "do_something"},
        {Type::OPEN_PAREN, "("},
        {Type::CLOSE_PAREN, ")"},
        {Type::SEMICOLON, ";"},
        {Type::CLOSE_BRACE, "}"},
        {Type::END, ""},
    });
}
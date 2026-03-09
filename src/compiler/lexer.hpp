#pragma once
#include <string_view>
#include <string>
#include <vector>

enum class Type
{
    DEFINE,
    INCLUDE,
    STRUCT,
    FUNCTION,
    VARIABLE,
    CONSTANT,
    INTERRUPT,
    ZEROPAGE,
    RETURN,
    IRQ,
    NMI,
    RESET,
    NUMBER,
    STRING,
    CHARACTER,
    IF,
    ELSE,
    WHILE,
    IDENTIFIER,
    OPEN_PAREN,
    CLOSE_PAREN,
    OPEN_BRACE,
    CLOSE_BRACE,
    OPEN_BRACKET,
    CLOSE_BRACKET,
    COLON,
    SEMICOLON,
    COMMA,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    EQUALS,
    ASSIGN,
    LESS_THAN,
    GREATER_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN_EQUAL,
    EXCLAIM,
    NOT_EQUAL,
    ARROW,
    AT,
    HASH,
    TYPE_UNSIGNED_8,
    TYPE_UNSIGNED_16,
    TYPE_UNSIGNED_32,
    TYPE_UNSIGNED_64,
    TYPE_SIGNED_8,
    TYPE_SIGNED_16,
    TYPE_SIGNED_32,
    TYPE_SIGNED_64,
    TYPE_BOOLEAN,
    TYPE_STRING,
    TYPE_PTR,
    TRUE,
    FALSE,
    // QUOTE,
    PERIOD,
    END = -1,
    UNKNOWN = -2, // should neva happen 
};

struct Token
{
    Type type;
    std::string_view value; // No more std::string allocations per token
    size_t line = 0;
    size_t column = 0;
    std::string to_string() const;
};

class Lexer
{
    std::string_view source;
    size_t cursor = 0;
    size_t line = 1;
    size_t column = 1;

public:
    explicit Lexer(std::string_view src) : source(src) {}
    std::vector<Token> tokenize();

private:
    void skip_whitespace_and_comments();
    constexpr bool is_eof() const { return cursor >= source.length(); }
    constexpr char peek() const { return is_eof() ? '\0' : source[cursor]; }
    inline void advance() {
        if (!is_eof() && source[cursor] == '\n') { line++; column = 1; }
        else { column++; }
        cursor++;
    }
};
#include "lexer.hpp"
#include <algorithm>
#include <stdio.h>
#include <cctype>
#include <assert.h>
#include <iostream>

void Lexer::skip_whitespace_and_comments() {
    while (!is_eof()) {
        if (std::isspace(peek())) {
            advance();
        } else if (source.substr(cursor).starts_with("//")) {
            while (!is_eof() && peek() != '\n') advance();
        } else {
            break;
        }
    }
}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;

    while (true) {
        skip_whitespace_and_comments();
        if (is_eof()) break;

        size_t start = cursor;
        


        if (std::isdigit(peek())) {
            if (source.substr(cursor).starts_with("0x")) {
                cursor += 2;
                while (!is_eof() && std::isxdigit(peek())) advance();
            } else if (source.substr(cursor).starts_with("0b")) {
                cursor += 2;
                while (!is_eof() && (peek() == '0' || peek() == '1')) advance();
            } else {
                while (!is_eof() && std::isdigit(peek())) advance();
            }
            tokens.push_back({Type::NUMBER, source.substr(start, cursor - start)});
            continue;
        }


        char c = peek();

        if (c == '"') {
            advance(); // make sure we dont end a string immediately
            while (!is_eof() && (peek() != '"')) {
                advance();
            }
            const auto length = cursor - start - 1;
            std::string_view str = source.substr(start + 1, length);
            tokens.push_back({Type::STRING, str});
            advance();
            continue;
        }

        if (std::isalnum(c) || c == '_' || c == '#') {
            while (!is_eof() && (std::isalnum(peek()) || peek() == '_' || peek() == '#')) {
                advance();
            }
            
            std::string_view word = source.substr(start, cursor - start);
            
            if (word == "#define")          tokens.push_back({Type::DEFINE, word});
            else if (word == "#include")    tokens.push_back({Type::INCLUDE, word});
            else if (word == "struct")      tokens.push_back({Type::STRUCT, word});
            else if (word == "fn")          tokens.push_back({Type::FUNCTION, word});
            else if (word == "var")         tokens.push_back({Type::VARIABLE, word});
            else if (word == "const")       tokens.push_back({Type::CONSTANT, word});
            else if (word == "__interrupt") tokens.push_back({Type::INTERRUPT, word});
            else if (word == "__zeropage")  tokens.push_back({Type::ZEROPAGE, word});
            else if (word == "u8")          tokens.push_back({Type::TYPE_UNSIGNED_8, word});
            else if (word == "u16")         tokens.push_back({Type::TYPE_UNSIGNED_16, word});
            else if (word == "u32")         tokens.push_back({Type::TYPE_UNSIGNED_32, word});
            else if (word == "u64")         tokens.push_back({Type::TYPE_UNSIGNED_64, word});
            else if (word == "i8")          tokens.push_back({Type::TYPE_SIGNED_8, word});
            else if (word == "i16")         tokens.push_back({Type::TYPE_SIGNED_16, word});
            else if (word == "i32")         tokens.push_back({Type::TYPE_SIGNED_32, word});
            else if (word == "i64")         tokens.push_back({Type::TYPE_SIGNED_64, word});
            else if (word == "bool")        tokens.push_back({Type::TYPE_BOOLEAN, word});
            else if (word == "str")         tokens.push_back({Type::TYPE_STRING, word});
            else if (word == "true")        tokens.push_back({Type::TRUE, word});
            else if (word == "false")       tokens.push_back({Type::FALSE, word});
            else                            tokens.push_back({Type::IDENTIFIER, word});
            continue;
        }

        std::string_view hi = source.substr(cursor);
        if (hi.starts_with("==")) {
            tokens.push_back({Type::EQUALS, hi.substr(0, 2)}),
            cursor += 2;
            continue;
        } else if (hi.starts_with(">=")) {
            tokens.push_back({Type::GREATER_THAN_EQUAL, hi.substr(0, 2)}),
            cursor += 2;
            continue;
        } else if (hi.starts_with("<=")) {
            tokens.push_back({Type::LESS_THAN_EQUAL, hi.substr(0, 2)}),
            cursor += 2;
            continue;
        } else if (hi.starts_with("!=")) {
            tokens.push_back({Type::NOT_EQUAL, hi.substr(0, 2)}),
            cursor += 2;
            continue;
        }

        c = peek(); // reassurance
        Type symType = Type::UNKNOWN;
        switch (c) {
            case ':': symType = Type::COLON; break;
            case ',': symType = Type::COMMA; break;
            case ';': symType = Type::SEMICOLON; break;
            case '+': symType = Type::PLUS; break;
            case '-': symType = Type::MINUS; break;
            case '*': symType = Type::ASTERISK; break;
            case '/': symType = Type::SLASH; break;
            case '(': symType = Type::OPEN_PAREN; break;
            case ')': symType = Type::CLOSE_PAREN; break;
            case '{': symType = Type::OPEN_BRACE; break;
            case '}': symType = Type::CLOSE_BRACE; break;
            case '[': symType = Type::OPEN_BRACKET; break;
            case ']': symType = Type::CLOSE_BRACKET; break;
            case '=': symType = Type::ASSIGN; break;
            case '.': symType = Type::PERIOD; break;
            case '<': symType = Type::LESS_THAN; break;
            case '>': symType = Type::GREATER_THAN; break;
            case '!': symType = Type::EXCLAIM; break;
            default: {
                symType = Type::UNKNOWN;
                std::cout << "Unknown character: " << c << std::endl;
            }
        }
        assert(symType != Type::UNKNOWN);
        
        tokens.push_back({symType, source.substr(cursor, 1)});
        advance();
    }



    tokens.push_back({Type::END, ""});
    return tokens;
}
#include "lexer.hpp"
#include <algorithm>
#include <stdio.h>
#include <cctype>
#include <assert.h>
#include <iostream>

void Lexer::skip_whitespace_and_comments()
{
    while (!is_eof())
    {
        if (std::isspace(peek()))
        {
            advance();
        }
        else if (source.substr(cursor).starts_with("//"))
        {
            while (!is_eof() && peek() != '\n')
                advance();
        }
        else
        {
            break;
        }
    }
}

std::vector<Token> Lexer::tokenize()
{
    std::vector<Token> tokens;

    size_t token_line = 1, token_col = 1;
    auto push = [&](Type t, std::string_view v) {
        tokens.push_back({t, v, token_line, token_col});
    };

    while (true)
    {
        skip_whitespace_and_comments();
        if (is_eof())
            break;

        token_line = line;
        token_col = column;
        size_t start = cursor;

        if (std::isdigit(peek()))
        {
            if (source.substr(cursor).starts_with("0x"))
            {
                cursor += 2; column += 2;
                while (!is_eof() && std::isxdigit(peek()))
                    advance();
            }
            else if (source.substr(cursor).starts_with("0b"))
            {
                cursor += 2; column += 2;
                while (!is_eof() && (peek() == '0' || peek() == '1'))
                    advance();
            }
            else
            {
                while (!is_eof() && std::isdigit(peek()))
                    advance();
            }
            push(Type::NUMBER, source.substr(start, cursor - start));
            continue;
        }

        char c = peek();

        if (c == '"')
        {
            advance(); // make sure we dont end a string immediately
            while (!is_eof() && (peek() != '"'))
            {
                advance();
            }
            const auto length = cursor - start - 1;
            std::string_view str = source.substr(start + 1, length);
            push(Type::STRING, str);
            advance();
            continue;
        }
        if (c == '#')
        {
            if (source.substr(cursor).starts_with("#include"))
            {
                cursor += 8; column += 8;
                push(Type::INCLUDE, "#include");
                continue;
            }
            else if (source.substr(cursor).starts_with("#define"))
            {
                cursor += 7; column += 7;
                push(Type::DEFINE, "#define");
                continue;
            }
            push(Type::HASH, "#");
            advance();
            continue;
        }
        if (c == '\'')
        {
            advance(); // skip opening '
            if (is_eof()) {
                throw std::runtime_error("Unterminated character literal");
            }
            size_t charStart = cursor;
            advance(); // consume character
            if (is_eof() || peek() != '\'') {
                throw std::runtime_error("Unterminated character literal");
            }
            advance(); // skip closing '
            push(Type::CHARACTER, source.substr(charStart, 1));
            continue;
        }

        if (std::isalnum(c) || c == '_')
        {
            while (!is_eof() && (std::isalnum(peek()) || peek() == '_'))
            {
                advance();
            }

            std::string_view word = source.substr(start, cursor - start);

            if (word == "struct")
                push(Type::STRUCT, word);
            else if (word == "fn")
                push(Type::FUNCTION, word);
            else if (word == "var")
                push(Type::VARIABLE, word);
            else if (word == "if")
                push(Type::IF, word);
            else if (word == "else")
                push(Type::ELSE, word);
            else if (word == "while")
                push(Type::WHILE, word);
            else if (word == "const")
                push(Type::CONSTANT, word);
            else if (word == "__interrupt")
                push(Type::INTERRUPT, word);
            else if (word == "__zeropage")
                push(Type::ZEROPAGE, word);
            else if (word == "nmi")
                push(Type::NMI, word);
            else if (word == "irq")
                push(Type::IRQ, word);
            else if (word == "reset")
                push(Type::RESET, word);
            else if (word == "u8")
                push(Type::TYPE_UNSIGNED_8, word);
            else if (word == "u16")
                push(Type::TYPE_UNSIGNED_16, word);
            else if (word == "u32")
                push(Type::TYPE_UNSIGNED_32, word);
            else if (word == "u64")
                push(Type::TYPE_UNSIGNED_64, word);
            else if (word == "i8")
                push(Type::TYPE_SIGNED_8, word);
            else if (word == "i16")
                push(Type::TYPE_SIGNED_16, word);
            else if (word == "i32")
                push(Type::TYPE_SIGNED_32, word);
            else if (word == "i64")
                push(Type::TYPE_SIGNED_64, word);
            else if (word == "bool")
                push(Type::TYPE_BOOLEAN, word);
            else if (word == "str")
                push(Type::TYPE_STRING, word);
            else if (word == "ptr")
                push(Type::TYPE_PTR, word);
            else if (word == "true")
                push(Type::TRUE, word);
            else if (word == "false")
                push(Type::FALSE, word);
            else if (word == "return")
                push(Type::RETURN, word);
            else
                push(Type::IDENTIFIER, word);
            continue;
        }

        std::string_view hi = source.substr(cursor);
        if (hi.starts_with("=="))
        {
            push(Type::EQUALS, hi.substr(0, 2));
            cursor += 2; column += 2;
            continue;
        }
        else if (hi.starts_with(">="))
        {
            push(Type::GREATER_THAN_EQUAL, hi.substr(0, 2));
            cursor += 2; column += 2;
            continue;
        }
        else if (hi.starts_with("<="))
        {
            push(Type::LESS_THAN_EQUAL, hi.substr(0, 2));
            cursor += 2; column += 2;
            continue;
        }
        else if (hi.starts_with("!="))
        {
            push(Type::NOT_EQUAL, hi.substr(0, 2));
            cursor += 2; column += 2;
            continue;
        }
        else if (hi.starts_with("->")) // try to find unique symbol
        {
            push(Type::ARROW, hi.substr(0, 2));
            cursor += 2; column += 2;
            continue;
        }

        c = peek(); // reassurance
        Type symType = Type::UNKNOWN;
        switch (c)
        {
        case ':':
            symType = Type::COLON;
            break;
        case ',':
            symType = Type::COMMA;
            break;
        case ';':
            symType = Type::SEMICOLON;
            break;
        case '+':
            symType = Type::PLUS;
            break;
        case '-':
            symType = Type::MINUS;
            break;
        case '*':
            symType = Type::ASTERISK;
            break;
        case '/':
            symType = Type::SLASH;
            break;
        case '(':
            symType = Type::OPEN_PAREN;
            break;
        case ')':
            symType = Type::CLOSE_PAREN;
            break;
        case '{':
            symType = Type::OPEN_BRACE;
            break;
        case '}':
            symType = Type::CLOSE_BRACE;
            break;
        case '[':
            symType = Type::OPEN_BRACKET;
            break;
        case ']':
            symType = Type::CLOSE_BRACKET;
            break;
        case '=':
            symType = Type::ASSIGN;
            break;
        case '.':
            symType = Type::PERIOD;
            break;
        case '<':
            symType = Type::LESS_THAN;
            break;
        case '>':
            symType = Type::GREATER_THAN;
            break;
        case '!':
            symType = Type::EXCLAIM;
            break;
        case '#':
            symType = Type::HASH;
            break;
        case '@':
            symType = Type::AT;
            break;
        default:
        {
            symType = Type::UNKNOWN;
            std::cout << "Unknown character: " << c << std::endl;
        }
        }
        assert(symType != Type::UNKNOWN);

        push(symType, source.substr(cursor, 1));
        advance();
    }

    tokens.push_back({Type::END, "", line, column});
    return tokens;
}

std::string Token::to_string() const
{
    return "Token(Type: " + std::to_string(static_cast<int>(this->type)) + ", Value: \"" + std::string(this->value) + "\", " + std::to_string(line) + ":" + std::to_string(column) + ")";
}

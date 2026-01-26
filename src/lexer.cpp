#include <string_view>
#include <vector>
#include <cctype>
#include "lexer.hpp"

std::vector<Token> Lexer::tokenize()
{
    std::vector<Token> tokens;
    while (!source.empty())
    {
        // 1. Skip whitespace
        skip_whitespace();
        if (source.empty())
            break;
        // 2. Identify tokens
        if (isdigit(source[0]))
        {
            tokens.push_back(read_number());
        }
        else
        {
            tokens.push_back(read_identifier());
        }
    }
    tokens.push_back({Type::END, ""});
    return tokens;
}
void Lexer::skip_whitespace()
{
    size_t first_non_space = source.find_first_not_of(" \t\n\r");
    if (first_non_space != std::string_view::npos)
    {
        source.remove_prefix(first_non_space);
    }
    else
    {
        source = ""; // Only whitespace left
    }
}

Token Lexer::read_identifier()
{
    size_t end = source.find_first_of(" \t\n\r");
    std::string_view word = source.substr(0, end);

    Type t = Type::NUMBER; // Default
    if (word == "SET")
        t = Type::SET;
    else if (word == "ADD")
        t = Type::ADD;

    source.remove_prefix(word.size());
    return {t, word};
}

Token Lexer::read_number()
{
    size_t end = source.find_first_not_of("0123456789");
    std::string_view num = source.substr(0, end);
    source.remove_prefix(num.size());
    return {Type::NUMBER, num};
}

#include <string_view>
#include <vector>
#include <cctype>

enum class Type { SET, ADD, NUMBER, END };

struct Token {
    Type type;
    std::string_view value; // No allocation here!
};

class Lexer {
    std::string_view source;
    size_t cursor = 0;

public:
    Lexer(std::string_view src) : source(src) {}
    std::vector<Token> tokenize() {}
private:
    void skip_whitespace();
    Token read_identifier();
    Token read_number();
};
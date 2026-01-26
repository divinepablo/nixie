#include <string>
#include "lexer.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

std::string read_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }

    // Efficiently read the whole file into a string
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: nixie [filename]\n";
        return 1;
    }

    try {
        // 1. Read file (This string MUST stay in scope)
        std::string source_code = read_file(argv[1]);

        // 2. Lex (Pass the string_view)
        Lexer lexer(source_code);
        auto tokens = lexer.tokenize();
        // 3. Print tokens
        for (const auto& token : tokens) {
            std::cout << "Token(Type: " << static_cast<int>(token.type)
                      << ", Value: \"" << token.value << "\")\n";
        }

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
#include <string>
#include "lexer.hpp"

#include <iostream>
int __cdecl main() {
    std::string code = "SET 5 ADD 10";
    
    Lexer lexer(code);
    auto tokens = lexer.tokenize();
    // auto ast = parse(tokens);      // [SetNode(5), AddNode(10)]
    // generate(ast);                 // LDA #$05, ADC #$0a
    
    return 0;
}
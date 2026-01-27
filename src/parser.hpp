#pragma once
#include "lexer.hpp"
#include <stdexcept>
#include "ast.hpp"
#include <string>

class ProgramNode : public Node {
    public:
    std::vector<std::unique_ptr<Node>> statements;
    std::vector<std::string> includes;
    void accept(Visitor& v) override {
        v.visit(*this);
    }
};

class Parser
{
    Token currentToken;
    std::vector<Token> tokens;
    size_t position = 0;

    inline void consume(Type type)
    {
        if (currentToken.type == type)
        {
            position++;
            if (position < tokens.size())
                currentToken = tokens[position];
        }
        else
            throw std::runtime_error("Unexpected Token at position: " + std::to_string(position) + ", " + currentToken.to_string());
    }

    inline Token peek(int offset = 1) {
        if (position + offset >= tokens.size())
            return tokens.back();

        return tokens[position + offset];
    }

public:
    explicit Parser(const std::vector<Token>& toks) : tokens(toks)
    {
        currentToken = tokens[position];
    }

    ProgramNode* parseProgram();
    ProgramNode* parse() {return parseProgram();};
    const AstType parsePointer(Token typing);
    constexpr AstType parseType(Token typing);
    std::unique_ptr<Node> parseExpression();

    int parseNumber();
    std::unique_ptr<StringNode> parseString();
    bool parseBoolean();
    std::unique_ptr<CallNode> parseCall();
    std::unique_ptr<MemberReferenceNode> parseMemberReference();

    std::unique_ptr<VariableNode> parseVariableDeclaration(bool constant=false, bool zeropage=false);

    std::unique_ptr<Node> parseStatement();
    
    std::vector<std::unique_ptr<Node>> parseBlock();

    std::unique_ptr<FunctionNode> parseFunction(bool interrupt = false);
    std::unique_ptr<StructureNode> parseStructure();
    std::unique_ptr<StructureInitNode> parseStructureInit();
    std::unique_ptr<IfNode> parseIf();
    std::unique_ptr<WhileNode> parseWhile();
    
    std::unique_ptr<IncludeNode> parseInclude();
    std::unique_ptr<DefineNode> parseDefine();
    // std::unique_ptr<DefineNode> parseDefine();

    // --- Expression Parsing (Precedence Hierarchy) ---
    /*
        |Level|Precedence    |Operator       |Method               |
        |-----|--------------|---------------|---------------------|
        |1    |Lowest        |=              |parseAssignment()    |
        |2    |Comparison    |==, !=, <, >   |parseComparison()    |
        |3    |Add / Subtract|+, -           |parseAddition()      |
        |4    |Mult / Div    |*, /           |parseMultiplication()|
        |5    |Unary         |!, - (negative)|parseUnary()         |
        |6    |Highest       |(), Literals   |parsePrimary()       |

    */
    
    std::unique_ptr<Node> parseAssignment();
    
    // Comparisons (==, !=)
    std::unique_ptr<Node> parseComparison();

    // Binary Operations
    std::unique_ptr<Node> parseAdditive();
    std::unique_ptr<Node> parseMultiplicative();    

    // Unary Operators (!, -)
    std::unique_ptr<Node> parseUnary();
    
    // Primary Literals (Variables, Numbers, Strings, Parentheses)
    std::unique_ptr<Node> parsePrimary();
};
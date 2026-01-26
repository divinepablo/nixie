#include "parser.hpp"
#include <memory>
constexpr AstType Parser::parseType(Token typing)
{
    switch (typing.type)
    {
    case Type::TYPE_UNSIGNED_8:
        return AstType::Primitive(TypeKind::UNSIGNED_8);
    case Type::TYPE_UNSIGNED_16:
        return AstType::Primitive(TypeKind::UNSIGNED_16);
    case Type::TYPE_UNSIGNED_32:
        return AstType::Primitive(TypeKind::UNSIGNED_32);
    case Type::TYPE_UNSIGNED_64:
        return AstType::Primitive(TypeKind::UNSIGNED_64);
    case Type::TYPE_SIGNED_8:
        return AstType::Primitive(TypeKind::SIGNED_8);
    case Type::TYPE_SIGNED_16:
        return AstType::Primitive(TypeKind::SIGNED_16);
    case Type::TYPE_SIGNED_32:
        return AstType::Primitive(TypeKind::SIGNED_32);
    case Type::TYPE_SIGNED_64:
        return AstType::Primitive(TypeKind::SIGNED_64);
    case Type::TYPE_BOOLEAN:
        return AstType::Primitive(TypeKind::BOOLEAN);
    case Type::TYPE_STRING:
        return AstType::Primitive(TypeKind::STRING);
    case Type::IDENTIFIER:
        return AstType::Struct(typing.value);
    default:
            throw std::runtime_error("Unknown type");
    }
}

int Parser::parseNumber() // thanks gpt
{
    std::string_view sv = currentToken.value;
    consume(Type::NUMBER);

    int base = 10;
    std::size_t start = 0;
    char sign = 0;

    if (!sv.empty() && (sv[0] == '+' || sv[0] == '-'))
    {
        sign = sv[0];
        start = 1;
    }

    if (sv.size() > start + 1 && sv[start] == '0')
    {
        char p = sv[start + 1];
        if (p == 'x' || p == 'X')
        {
            base = 16;
            start += 2;
        }
        else if (p == 'b' || p == 'B')
        {
            base = 2;
            start += 2;
        }
        else if (p == 'o' || p == 'O')
        {
            base = 8;
            start += 2;
        }
    }

    std::string clean;
    clean.reserve(sv.size() - start + (sign ? 1 : 0));
    if (sign)
        clean.push_back(sign);
    for (std::size_t i = start; i < sv.size(); ++i)
    {
        char c = sv[i];
        if (c != '_')
            clean.push_back(c);
    }
    if (clean.size() == (sign ? 1 : 0))
    {
        throw std::runtime_error("Invalid numeric literal");
    }

    std::size_t pos = 0;
    try
    {
        int value = std::stoi(clean, &pos, base);
        if (pos != clean.size())
            throw std::runtime_error("Invalid numeric literal");
        return value;
    }
    catch (...)
    {
        throw std::runtime_error("Invalid numeric literal");
    }
}

std::unique_ptr<StringNode> Parser::parseString()
{
    const auto str = currentToken.value;
    consume(Type::STRING);
    return std::make_unique<StringNode>(StringNode(str));
}

bool Parser::parseBoolean()
{
    bool ret = false;
    if (currentToken.type == Type::TRUE)
    {
        ret = true;
    }
    else if (currentToken.type == Type::FALSE)
        ret = false;
    else
        throw std::runtime_error("Invalid boolean");
    consume(currentToken.type);
    return ret;
}

std::unique_ptr<Node> Parser::parseCall()
{
    std::string_view name = currentToken.value;
    consume(Type::IDENTIFIER);
    consume(Type::OPEN_PAREN);

    std::vector<std::unique_ptr<Node>> parameters;

    while (currentToken.type != Type::CLOSE_PAREN)
    {
        parameters.emplace_back(parseExpression());
        if (currentToken.type == Type::COMMA) consume(Type::COMMA);
    }
    consume(Type::CLOSE_PAREN);
    return std::make_unique<CallNode>(CallNode(name, std::move(parameters)));
}

std::unique_ptr<VariableNode> Parser::parseVariableDeclaration(bool zeropage)
{
    consume(Type::VARIABLE);
    std::string_view name = currentToken.value;
    std::string_view strutype = "";
    consume(Type::IDENTIFIER);
    consume(Type::COLON);
    auto type = parseType(currentToken);
    if (type == TypeKind::UNKNOWN)
        throw std::runtime_error("Unknown type");
    else if (type == TypeKind::STRUCTURE)
        strutype = currentToken.value;
    consume(currentToken.type);
    consume(Type::ASSIGN);
    return std::make_unique<VariableNode>(VariableNode(name, type, parseExpression(), zeropage, strutype));
}

std::unique_ptr<Node> Parser::parseStatement()
{
    switch (currentToken.type)
    {
    case Type::ZEROPAGE:
    {
        consume(Type::ZEROPAGE);

        auto vardeclelcdxa = parseVariableDeclaration(true);
        consume(Type::SEMICOLON);
        return std::move(vardeclelcdxa);
    }
    case Type::VARIABLE:
    {
        auto vardeclelcdxa = parseVariableDeclaration();
        consume(Type::SEMICOLON);
        return std::move(vardeclelcdxa);
    }
    case Type::IF: return parseIf();
    case Type::WHILE: return parseWhile();
    
    default:
        auto expr = parseAssignment();
        consume(Type::SEMICOLON);
        return std::move(expr);
    }
}

std::vector<std::unique_ptr<Node>> Parser::parseBlock()
{
    consume(Type::OPEN_BRACE);
    std::vector<std::unique_ptr<Node>> nodes;
    while (currentToken.type != Type::CLOSE_BRACE) {
        nodes.push_back(parseStatement());
    }

    consume(Type::CLOSE_BRACE);
    return nodes;
}

std::unique_ptr<FunctionNode> Parser::parseFunction(bool interrupt)
{
    if (interrupt) consume(Type::INTERRUPT);
    consume(Type::FUNCTION);

    const auto name = currentToken.value;
    consume(Type::IDENTIFIER);

    consume(Type::OPEN_PAREN);

    std::map<std::string_view, AstType> parameters;

    while (currentToken.type != Type::CLOSE_PAREN)
    {
        const auto param = currentToken.value;
        consume(Type::IDENTIFIER);
        consume(Type::COLON);
        
        auto type = parseType(currentToken);
        consume(currentToken.type);
        if (currentToken.type == Type::COMMA) consume(Type::COMMA);
        parameters.emplace(std::make_pair(param, type));
    }
    consume(Type::CLOSE_PAREN);

    auto block = parseBlock();

    return std::make_unique<FunctionNode>(FunctionNode(name, std::move(parameters), std::move(block), interrupt));
}

std::unique_ptr<StructureNode> Parser::parseStructure()
{
    consume(Type::STRUCT);

    const auto name = currentToken.value;
    consume(Type::IDENTIFIER);

    consume(Type::OPEN_BRACE);

    std::map<std::string_view, AstType> parameters;

    while (currentToken.type != Type::CLOSE_BRACE)
    {
        const auto param = currentToken.value;
        consume(Type::IDENTIFIER);
        consume(Type::COLON);
        
        auto type = parseType(currentToken);
        consume(currentToken.type);
        if (currentToken.type == Type::COMMA) consume(Type::COMMA);
        parameters.emplace(std::make_pair(param, type));
    }
    consume(Type::CLOSE_BRACE);


    return std::make_unique<StructureNode>(StructureNode(name, std::move(parameters)));
}

ProgramNode *Parser::parseProgram()
{
    ProgramNode *root = new ProgramNode();

    while (currentToken.type != Type::END)
    {
        switch (currentToken.type)
        {
        case Type::INCLUDE:
            root->statements.push_back(parseInclude());
            break;
        case Type::FUNCTION:
            root->statements.push_back(parseFunction());
            break;
        case Type::INTERRUPT:
            root->statements.push_back(parseFunction(true));
            break;
        case Type::STRUCT:
            root->statements.push_back(parseStructure());
            break;
        default:
            root->statements.push_back(parseStatement());
            break;
        }
    }
    return root;
}

std::unique_ptr<IfNode> Parser::parseIf()
{
    consume(Type::IF);
        consume(Type::OPEN_PAREN);
        auto condition = parseExpression();
        consume(Type::CLOSE_PAREN);
        auto block = parseBlock();
        return std::make_unique<IfNode>(IfNode(std::move(condition), std::move(block)));
}

std::unique_ptr<WhileNode> Parser::parseWhile()
{
    consume(Type::WHILE);
    consume(Type::OPEN_PAREN);
    auto condition = parseExpression();
    consume(Type::CLOSE_PAREN);
    auto block = parseBlock();
    return std::make_unique<WhileNode>(WhileNode(std::move(condition), std::move(block)));
}

std::unique_ptr<IncludeNode> Parser::parseInclude()
{
    consume(Type::INCLUDE);
    return std::make_unique<IncludeNode>(IncludeNode(parseString()));
}

std::unique_ptr<Node> Parser::parseExpression()
{
    return parseAssignment(); // idk big gemi told me to do this
}

std::unique_ptr<Node> Parser::parseAssignment()
{
    auto left = parseComparison();

    if (currentToken.type == Type::ASSIGN)
    {
        consume(Type::EQUALS);
        auto right = parseAssignment();
        return std::make_unique<AssignmentNode>(AssignmentNode(std::move(left), std::move(right)));
    }

    return left;
}
std::unique_ptr<Node> Parser::parseComparison()
{
    auto left = parseAdditive();

    if (currentToken.type == Type::LESS_THAN || currentToken.type == Type::LESS_THAN_EQUAL || currentToken.type == Type::GREATER_THAN || currentToken.type == Type::GREATER_THAN_EQUAL || currentToken.type == Type::EQUALS || currentToken.type == Type::NOT_EQUAL)
    {
        Token operate = currentToken;
        consume(operate.type);
        auto right = parseAdditive();

        auto comparison = tokenToComparison(operate);
        return std::make_unique<ComparisonNode>(ComparisonNode(comparison, std::move(left), std::move(right)));
    }

    return left;
}

std::unique_ptr<Node> Parser::parseAdditive()
{
    auto left = parseMultiplicative();

    if (currentToken.type == Type::PLUS || currentToken.type == Type::MINUS)
    {
        Token operate = currentToken;
        consume(operate.type);
        auto right = parseMultiplicative();

        auto operatr = tokenToOperator(operate);
        return std::make_unique<BinaryOperationNode>(BinaryOperationNode(operatr, std::move(left), std::move(right)));
    }

    return left;
}

std::unique_ptr<Node> Parser::parseMultiplicative()
{
    auto left = parseUnary();

    if (currentToken.type == Type::ASTERISK || currentToken.type == Type::SLASH)
    {
        Token operate = currentToken;
        consume(operate.type);
        auto right = parseUnary();

        auto operatr = tokenToOperator(operate);
        return std::make_unique<BinaryOperationNode>(BinaryOperationNode(operatr, std::move(left), std::move(right)));
    }

    return left;
}

std::unique_ptr<Node> Parser::parseUnary()
{
    if (currentToken.type == Type::EXCLAIM || currentToken.type == Type::MINUS)
    {
        Token op = currentToken;
        consume(op.type);
        auto operand = parseUnary();
        const auto operate = (op.type == Type::EXCLAIM) ? UnaryOperator::NOT : UnaryOperator::NEGATE;
        return std::make_unique<UnaryNode>(UnaryNode(operate, std::move(operand)));
    }
    return parsePrimary();
}

std::unique_ptr<Node> Parser::parsePrimary()
{
    switch (currentToken.type)
    {
    case Type::NUMBER:
    {
        return std::make_unique<NumberNode>(NumberNode(parseNumber()));
    }
    case Type::STRING:
    {
        return parseString();
    }
    case Type::FALSE:
    case Type::TRUE:
    {
        return std::make_unique<BoolNode>(BoolNode(parseBoolean()));
    }
    case Type::IDENTIFIER:
    {
        if (peek().type == Type::OPEN_PAREN)
        {
            return parseCall();
        }
        else
        {
            std::string_view name = currentToken.value;

            consume(Type::IDENTIFIER);
            return std::make_unique<ReferenceNode>(ReferenceNode(name));
        }
    }
    default:
        throw std::runtime_error("Unknown expression");
    }
}

// std::unique_ptr<ComparisonNode> Parser::parseComparison()
// {
//     return std::make_unique<ComparisonNode>();
// }

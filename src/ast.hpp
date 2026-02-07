#pragma once
#include <string_view>
#include <vector>
#include <map>
#include <memory>
#include "lexer.hpp"
enum class Operator
{
    ADD = 1,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    UNKNOWN = -1,
}; // this language's syntax is odd.

enum class UnaryOperator
{
    NOT = 1,
    NEGATE,
    DEREFERENCE,
    REFERENCE,
    UNKNOWN,
}; // this language's syntax is odd.

enum class InterruptType
{
    NONE = 0,    // Not an interrupt handler
    NMI,         // Non-Maskable Interrupt ($FFFA)
    RESET,       // Reset vector ($FFFC)
    IRQ,         // IRQ/BRK vector ($FFFE)
};

enum class TypeKind // thanks big gemi
{
    UNSIGNED_8,
    UNSIGNED_16,
    UNSIGNED_32,
    UNSIGNED_64,
    SIGNED_8,
    SIGNED_16,
    SIGNED_32,
    SIGNED_64,
    BOOLEAN,
    STRING,
    STRUCTURE, // This indicates we need to look at the 'name' field
    UNKNOWN = -1,
};

class Visitor; // Forward declaration
struct IncludeNode;
struct DefineNode;
struct StringNode;
struct NumberNode;
struct BoolNode;
struct AssignmentNode;
struct BinaryOperationNode;
struct ComparisonNode;
struct FunctionNode;
struct StructureNode;
struct StructureInitNode;
struct VariableNode;
struct ReferenceNode;
struct CallNode;
struct IfNode;
struct UnaryNode;
struct WhileNode;
struct MemberReferenceNode;
struct ReturnNode;
struct CrescereNode;

// 2. Create the unified Type struct
struct AstType {
    TypeKind kind;
    std::string_view name; // Only used if kind == TypeKind::STRUCTURE
    unsigned int pointerLevel = 0; // 0 = not a pointer, 1 = T*, 2 = T**, ...

    // Helpers to make creation easier
    static constexpr AstType Primitive(const TypeKind k, unsigned int ptr = 0) { return { k, "", ptr }; }
    static constexpr AstType Struct(std::string_view n, unsigned int ptr = 0) { return { TypeKind::STRUCTURE, n, ptr }; }
    static constexpr AstType PointerTo(const AstType& base, unsigned int levels = 1) {
        AstType t = base;
        t.pointerLevel += levels;
        return t;
    }

    bool isPointer() const { return pointerLevel > 0; }
    AstType decayPointer(unsigned int levels = 1) const {
        AstType t = *this;
        t.pointerLevel = (t.pointerLevel > levels) ? (t.pointerLevel - levels) : 0;
        return t;
    }

    // Equality compares full type (including pointer level and struct name)
    bool operator==(const AstType& other) const {
        return kind == other.kind && name == other.name && pointerLevel == other.pointerLevel;
    }
    bool operator!=(const AstType& other) const { return !(*this == other); }

    // Convenience: compare base kind (ignores pointer level and struct name)
    bool operator==(TypeKind k) const { return kind == k; }
    bool operator!=(TypeKind k) const { return kind != k; }
};

enum class Comparison
{
    EQUALS = 1,
    LESS_THAN,
    GREATER_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN_EQUAL,
    NOT_EQUAL,
    UNKNOWN = -1,
};

struct Node
{
    virtual void accept(Visitor &v) = 0;
    virtual ~Node() = default;
};

class Visitor
{
public:
    virtual void visit(Node &node) = 0;
    virtual void visit(IncludeNode &node) = 0;
    virtual void visit(DefineNode &node) = 0;
    virtual void visit(StringNode &node) = 0;
    virtual void visit(NumberNode &node) = 0;
    virtual void visit(UnaryNode &node) = 0;
    virtual void visit(BoolNode &node) = 0;
    virtual void visit(AssignmentNode &node) = 0;
    virtual void visit(BinaryOperationNode &node) = 0;
    virtual void visit(ComparisonNode &node) = 0;
    virtual void visit(FunctionNode &node) = 0;
    virtual void visit(StructureNode &node) = 0;
    virtual void visit(StructureInitNode &node) = 0;
    virtual void visit(VariableNode &node) = 0;
    virtual void visit(ReferenceNode &node) = 0;
    virtual void visit(CallNode &node) = 0;
    virtual void visit(IfNode &node) = 0;
    virtual void visit(WhileNode &node) = 0;
    virtual void visit(MemberReferenceNode &node) = 0;
    virtual void visit(ReturnNode &node) = 0;
    virtual void visit(CrescereNode &node) = 0;
};

constexpr Comparison tokenToComparison(const Token token) {
    switch (token.type) {
        case Type::EQUALS            : return Comparison::EQUALS            ;
        case Type::LESS_THAN         : return Comparison::LESS_THAN         ;
        case Type::GREATER_THAN      : return Comparison::GREATER_THAN      ;
        case Type::LESS_THAN_EQUAL   : return Comparison::LESS_THAN_EQUAL   ;
        case Type::GREATER_THAN_EQUAL: return Comparison::GREATER_THAN_EQUAL;
        case Type::NOT_EQUAL         : return Comparison::NOT_EQUAL         ;
        default                      : return Comparison::UNKNOWN           ;
    }
}

constexpr Operator tokenToOperator(const Token token) {
    switch (token.type) {
        case Type::PLUS     : return Operator::ADD      ;
        case Type::MINUS    : return Operator::SUBTRACT ;
        case Type::ASTERISK : return Operator::MULTIPLY ;
        case Type::SLASH    : return Operator::DIVIDE   ;
        default             : return Operator::UNKNOWN  ;
    }
}

constexpr UnaryOperator tokenToUnary(const Token token) {
    switch (token.type) {
        case Type::EXCLAIM  : return UnaryOperator::NOT         ;
        case Type::MINUS    : return UnaryOperator::NEGATE      ;
        case Type::HASH     : return UnaryOperator::REFERENCE   ;
        case Type::AT       : return UnaryOperator::DEREFERENCE ;
        default             : return UnaryOperator::UNKNOWN     ;
    }
}

struct IncludeNode : Node
{
    std::unique_ptr<StringNode> value;
    explicit IncludeNode(std::unique_ptr<StringNode> val) : value(std::move(val)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct ReferenceNode : Node
{
    std::string_view name;
    explicit ReferenceNode(std::string_view val) : name(val) {}
    void accept(Visitor &v) override { v.visit(*this); }
};
struct MemberReferenceNode : Node
{
    // std::unique_ptr<Node> base;
    std::string_view base;
    std::string_view memberName;
    explicit MemberReferenceNode(std::string_view base, std::string_view val) : /*base(std::move(base)),*/ base(base), memberName(val) {}
    void accept(Visitor &v) override { v.visit(*this); }
};
struct CallNode : Node
{
    std::string_view name;
    std::vector<std::unique_ptr<Node>> parameters;
    explicit CallNode(std::string_view val, std::vector<std::unique_ptr<Node>> parameters) : name(val), parameters(std::move(parameters)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct DefineNode : Node
{
    std::string_view name;
    std::unique_ptr<Node> expression;
    explicit DefineNode(std::string_view name, std::unique_ptr<Node> expression) : name(name), expression(std::move(expression)) {}
    virtual void accept(Visitor &v) override { v.visit(*this); }
};

struct StringNode : Node
{
    std::string_view value;
    explicit StringNode(std::string_view val) : value(val) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct NumberNode : Node
{
    std::int32_t value;
    explicit NumberNode(std::int32_t val) : value(val) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct BoolNode : Node
{
    bool value;
    explicit BoolNode(bool val) : value(val) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct AssignmentNode : Node
{

    std::unique_ptr<Node> assignee;
    std::unique_ptr<Node> expression;
    explicit AssignmentNode(std::unique_ptr<Node> assign, std::unique_ptr<Node> expr) : assignee(std::move(assign)), expression(std::move(expr)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct BinaryOperationNode : Node
{
    Operator operation;
    std::unique_ptr<Node> left, right;
    explicit BinaryOperationNode(Operator operation, std::unique_ptr<Node> left, std::unique_ptr<Node> right) : operation(operation), left(std::move(left)), right(std::move(right)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct UnaryNode : Node
{
    UnaryOperator operation;
    std::unique_ptr<Node> expression;
    explicit UnaryNode(UnaryOperator operation, std::unique_ptr<Node> expression) : operation(operation), expression(std::move(expression)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct ComparisonNode : Node
{
    Comparison comparison;

    std::unique_ptr<Node> left, right;
    explicit ComparisonNode(Comparison operation, std::unique_ptr<Node> left, std::unique_ptr<Node> right) : comparison(operation), left(std::move(left)), right(std::move(right)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct FunctionNode : Node
{
    std::string_view name;
    std::map<std::string_view, AstType> parameters;
    std::vector<std::unique_ptr<Node>> body;
    InterruptType interruptType;
    bool defined;
    explicit FunctionNode(std::string_view id, std::map<std::string_view, AstType> parameters, std::vector<std::unique_ptr<Node>> nodes, InterruptType interrupt = InterruptType::NONE) : name(id), parameters(std::move(parameters)), body(std::move(nodes)), interruptType(interrupt), defined(true) {}
    explicit FunctionNode(std::string_view id, std::map<std::string_view, AstType> parameters, InterruptType interrupt = InterruptType::NONE) : name(id), parameters(std::move(parameters)), interruptType(interrupt), defined(false) {}
    void accept(Visitor &v) override { v.visit(*this); }
    bool isInterrupt() const { return interruptType != InterruptType::NONE; }
};

struct IfNode : Node
{
    std::unique_ptr<Node> condition;
    std::vector<std::unique_ptr<Node>> body;
    std::vector<std::unique_ptr<Node>> elseBody;
    explicit IfNode(std::unique_ptr<Node> cond, std::vector<std::unique_ptr<Node>> nodes, std::vector<std::unique_ptr<Node>> elseNodes = {}) : condition(std::move(cond)), body(std::move(nodes)), elseBody(std::move(elseNodes)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct WhileNode : Node
{
    std::unique_ptr<Node> condition;
    std::vector<std::unique_ptr<Node>> body;
    explicit WhileNode(std::unique_ptr<Node> cond, std::vector<std::unique_ptr<Node>> nodes) : condition(std::move(cond)), body(std::move(nodes)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct StructureNode : Node
{
    std::string_view name;
    std::map<std::string_view, AstType> members;
    explicit StructureNode(std::string_view id, std::map<std::string_view, AstType> members) : name(id), members(std::move(members)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};
struct StructureInitNode : Node
{
    std::map<std::string_view, std::unique_ptr<Node>> members;
    explicit StructureInitNode(std::map<std::string_view, std::unique_ptr<Node>> members) : members(std::move(members)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct VariableNode : Node
{
    std::string_view name;
    AstType type;
    std::unique_ptr<Node> value;
    bool constant;
    bool zeropaged;
    std::string_view structureName;
    explicit VariableNode(std::string_view identifier, AstType type, std::unique_ptr<Node> expr, bool constant = false, bool zeropage = false, std::string_view structName = "") : name(identifier), type(type), value(std::move(expr)), constant(constant), zeropaged(zeropage), structureName(structName) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct ReturnNode : Node
{
    std::unique_ptr<Node> expression; // nullptr for void return
    explicit ReturnNode(std::unique_ptr<Node> expr = nullptr) : expression(std::move(expr)) {}
    void accept(Visitor &v) override { v.visit(*this); }
};
struct CrescereNode : Node
{
    std::unique_ptr<Node> expression; // nullptr for void return
    bool decrement = false;
    bool pre = true;
    explicit CrescereNode(std::unique_ptr<Node> expr = nullptr, bool dec = false, bool isPre = true) : expression(std::move(expr)), decrement(dec), pre(isPre) {}
    void accept(Visitor &v) override { v.visit(*this); }
};
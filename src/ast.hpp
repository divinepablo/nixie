#include <string_view>
#include <vector>
#include <map>
#include <memory>

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
struct VariableNode;

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
}; // this language's syntax is odd.

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

// 2. Create the unified Type struct
struct AstType {
    TypeKind kind;
    std::string_view name; // Only used if kind == TypeKind::STRUCTURE

    // Helpers to make creation easier
    static AstType Primitive(const TypeKind k) { _ASSERT(k != TypeKind::STRUCTURE); return { k, "" }; }
    static AstType Struct(std::string_view name) { return { TypeKind::STRUCTURE, name }; }
    
    // Allow comparing directly against the enum for convenience
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
    virtual void visit(BoolNode &node) = 0;
    virtual void visit(AssignmentNode &node) = 0;
    virtual void visit(BinaryOperationNode &node) = 0;
    virtual void visit(ComparisonNode &node) = 0;
    virtual void visit(FunctionNode &node) = 0;
    virtual void visit(StructureNode &node) = 0;
    virtual void visit(VariableNode &node) = 0;
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
    int value;
    explicit NumberNode(int val) : value(val) {}
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
    bool interrupt;
    explicit FunctionNode(std::string_view id, std::map<std::string_view, AstType> parameters, std::vector<std::unique_ptr<Node>> nodes, bool interrupt = false) : name(id), parameters(std::move(parameters)), body(std::move(nodes)), interrupt(interrupt) {}
    void accept(Visitor &v) override { v.visit(*this); }
};

struct IfNode : Node
{
    std::unique_ptr<Node> condition;
    std::vector<std::unique_ptr<Node>> body;
    explicit IfNode(std::unique_ptr<Node> cond, std::vector<std::unique_ptr<Node>> nodes) : condition(std::move(cond)), body(std::move(nodes)) {}
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

struct VariableNode : Node
{
    std::string_view name;
    AstType type;
    std::unique_ptr<Node> value;
    bool zeropaged;
    std::string_view structureName;
    explicit VariableNode(std::string_view identifier, AstType type, std::unique_ptr<Node> expr, bool zeropage = false, std::string_view structName = "") : name(identifier), type(type), value(std::move(expr)), zeropaged(zeropage), structureName(structName) {}
    void accept(Visitor &v) override { v.visit(*this); }
};
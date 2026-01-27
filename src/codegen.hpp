#include "lexer.hpp"
#include "ast.hpp"
#include "o65.hpp"
#include <vector>
#include <map>
#include <string>
#include <cstdint>

class CodegenVisitor : public Visitor
{
private:
public:
    void visit(Node &node) override;
    void visit(IncludeNode &node) override;
    void visit(DefineNode &node) override;
    void visit(StringNode &node) override;
    void visit(NumberNode &node) override;
    void visit(UnaryNode &node) override;
    void visit(BoolNode &node) override;
    void visit(AssignmentNode &node) override;
    void visit(BinaryOperationNode &node) override;
    void visit(ComparisonNode &node) override;
    void visit(FunctionNode &node) override;
    void visit(StructureNode &node) override;
    void visit(StructureInitNode &node) override;
    void visit(VariableNode &node) override;
    void visit(ReferenceNode &node) override;
    void visit(CallNode &node) override;
    void visit(IfNode &node) override;
    void visit(WhileNode &node) override;
    void visit(MemberReferenceNode &node) override;
};

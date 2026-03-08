#pragma once
#include "ast.hpp"
#include "parser.hpp"
#include <iostream>
#include <string>

class AstPrinter : public Visitor {
    int depth = 0;

    void indent() {
        for (int i = 0; i < depth; i++) std::cout << "  ";
    }

    static const char* typeKindStr(TypeKind k) {
        switch (k) {
            case TypeKind::UNSIGNED_8:  return "u8";
            case TypeKind::UNSIGNED_16: return "u16";
            case TypeKind::UNSIGNED_32: return "u32";
            case TypeKind::UNSIGNED_64: return "u64";
            case TypeKind::SIGNED_8:   return "i8";
            case TypeKind::SIGNED_16:  return "i16";
            case TypeKind::SIGNED_32:  return "i32";
            case TypeKind::SIGNED_64:  return "i64";
            case TypeKind::BOOLEAN:    return "bool";
            case TypeKind::STRING:     return "string";
            case TypeKind::STRUCTURE:  return "struct";
            default:                   return "unknown";
        }
    }

    std::string typeStr(const AstType& t) {
        std::string s;
        if (t.kind == TypeKind::STRUCTURE)
            s = std::string("struct ") + std::string(t.name);
        else
            s = typeKindStr(t.kind);
        for (unsigned int i = 0; i < t.pointerLevel; i++) s += "*";
        return s;
    }

    static const char* operatorStr(Operator op) {
        switch (op) {
            case Operator::ADD:      return "+";
            case Operator::SUBTRACT: return "-";
            case Operator::MULTIPLY: return "*";
            case Operator::DIVIDE:   return "/";
            default:                 return "?";
        }
    }

    static const char* unaryStr(UnaryOperator op) {
        switch (op) {
            case UnaryOperator::NOT:         return "!";
            case UnaryOperator::NEGATE:      return "-";
            case UnaryOperator::REFERENCE:   return "#";
            case UnaryOperator::DEREFERENCE: return "@";
            default:                         return "?";
        }
    }

    static const char* comparisonStr(Comparison c) {
        switch (c) {
            case Comparison::EQUALS:             return "==";
            case Comparison::NOT_EQUAL:          return "!=";
            case Comparison::LESS_THAN:          return "<";
            case Comparison::GREATER_THAN:       return ">";
            case Comparison::LESS_THAN_EQUAL:    return "<=";
            case Comparison::GREATER_THAN_EQUAL: return ">=";
            default:                             return "?";
        }
    }

    static const char* interruptStr(InterruptType t) {
        switch (t) {
            case InterruptType::NMI:   return "NMI";
            case InterruptType::RESET: return "RESET";
            case InterruptType::IRQ:   return "IRQ";
            default:                   return "NONE";
        }
    }

public:
    void visit(Node& node) override {
        indent();
        std::cout << "Node (generic)\n";
    }

    void visit(IncludeNode& node) override {
        indent();
        std::cout << "Include: \"" << node.value->value << "\"\n";
    }

    void visit(DefineNode& node) override {
        indent();
        std::cout << "Define: " << node.name << "\n";
        depth++;
        node.expression->accept(*this);
        depth--;
    }

    void visit(StringNode& node) override {
        indent();
        std::cout << "String: \"" << node.value << "\"\n";
    }

    void visit(NumberNode& node) override {
        indent();
        std::cout << "Number: " << node.value << "\n";
    }

    void visit(BoolNode& node) override {
        indent();
        std::cout << "Bool: " << (node.value ? "true" : "false") << "\n";
    }

    void visit(UnaryNode& node) override {
        indent();
        std::cout << "Unary: " << unaryStr(node.operation) << "\n";
        depth++;
        node.expression->accept(*this);
        depth--;
    }

    void visit(AssignmentNode& node) override {
        indent();
        std::cout << "Assignment\n";
        depth++;
        indent(); std::cout << "Target:\n";
        depth++;
        node.assignee->accept(*this);
        depth--;
        indent(); std::cout << "Value:\n";
        depth++;
        node.expression->accept(*this);
        depth--;
        depth--;
    }

    void visit(BinaryOperationNode& node) override {
        indent();
        std::cout << "BinaryOp: " << operatorStr(node.operation) << "\n";
        depth++;
        node.left->accept(*this);
        node.right->accept(*this);
        depth--;
    }

    void visit(ComparisonNode& node) override {
        indent();
        std::cout << "Comparison: " << comparisonStr(node.comparison) << "\n";
        depth++;
        node.left->accept(*this);
        node.right->accept(*this);
        depth--;
    }

    void visit(FunctionNode& node) override {
        indent();
        std::cout << "Function: " << node.name;
        if (node.isInterrupt()) std::cout << " [interrupt=" << interruptStr(node.interruptType) << "]";
        if (!node.defined) std::cout << " (declaration only)";
        std::cout << "\n";
        if (!node.parameters.empty()) {
            depth++;
            indent(); std::cout << "Params:\n";
            depth++;
            for (auto& [name, type] : node.parameters) {
                indent();
                std::cout << name << ": " << typeStr(type) << "\n";
            }
            depth--;
            depth--;
        }
        if (node.defined) {
            depth++;
            indent(); std::cout << "Body:\n";
            depth++;
            for (auto& stmt : node.body) stmt->accept(*this);
            depth--;
            depth--;
        }
    }

    void visit(StructureNode& node) override {
        indent();
        std::cout << "Struct: " << node.name << "\n";
        depth++;
        for (auto& [name, type] : node.members) {
            indent();
            std::cout << name << ": " << typeStr(type) << "\n";
        }
        depth--;
    }

    void visit(StructureInitNode& node) override {
        indent();
        std::cout << "StructInit\n";
        depth++;
        for (auto& [name, val] : node.members) {
            indent();
            std::cout << name << ":\n";
            depth++;
            val->accept(*this);
            depth--;
        }
        depth--;
    }

    void visit(VariableNode& node) override {
        indent();
        std::cout << "Variable: " << node.name << " (" << typeStr(node.type) << ")";
        if (node.constant) std::cout << " [const]";
        if (node.zeropaged) std::cout << " [zeropage]";
        if (!node.structureName.empty()) std::cout << " [struct=" << node.structureName << "]";
        std::cout << "\n";
        if (node.value) {
            depth++;
            node.value->accept(*this);
            depth--;
        }
    }

    void visit(ReferenceNode& node) override {
        indent();
        std::cout << "Reference: " << node.name << "\n";
    }

    void visit(CallNode& node) override {
        indent();
        std::cout << "Call: " << node.name << "\n";
        if (!node.parameters.empty()) {
            depth++;
            indent(); std::cout << "Args:\n";
            depth++;
            for (auto& arg : node.parameters) arg->accept(*this);
            depth--;
            depth--;
        }
    }

    void visit(IfNode& node) override {
        indent();
        std::cout << "If\n";
        depth++;
        indent(); std::cout << "Condition:\n";
        depth++;
        node.condition->accept(*this);
        depth--;
        indent(); std::cout << "Then:\n";
        depth++;
        for (auto& stmt : node.body) stmt->accept(*this);
        depth--;
        if (!node.elseBody.empty()) {
            indent(); std::cout << "Else:\n";
            depth++;
            for (auto& stmt : node.elseBody) stmt->accept(*this);
            depth--;
        }
        depth--;
    }

    void visit(WhileNode& node) override {
        indent();
        std::cout << "While\n";
        depth++;
        indent(); std::cout << "Condition:\n";
        depth++;
        node.condition->accept(*this);
        depth--;
        indent(); std::cout << "Body:\n";
        depth++;
        for (auto& stmt : node.body) stmt->accept(*this);
        depth--;
        depth--;
    }

    void visit(MemberReferenceNode& node) override {
        indent();
        std::cout << "MemberRef: " << node.base << "." << node.memberName << "\n";
    }

    void visit(ReturnNode& node) override {
        indent();
        std::cout << "Return";
        if (node.expression) {
            std::cout << "\n";
            depth++;
            node.expression->accept(*this);
            depth--;
        } else {
            std::cout << " (void)\n";
        }
    }

    void visit(CrescereNode& node) override {
        indent();
        std::cout << (node.decrement ? "Decrement" : "Increment");
        std::cout << (node.pre ? " (pre)" : " (post)") << "\n";
        if (node.expression) {
            depth++;
            node.expression->accept(*this);
            depth--;
        }
    }
};

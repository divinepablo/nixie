#pragma once
#include <gtest/gtest.h>
#include "codegen.hpp"
#include "ast.hpp"
#include "opcodes.h"
#include "o65.hpp"
#include <vector>
#include <memory>
#include <optional>
#include <string>
#include <map>
#include <iomanip>
#include <sstream>

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * @brief Compare byte vectors with detailed error messages
 */
inline void expectBytes(const std::vector<uint8_t>& actual, 
                        const std::vector<uint8_t>& expected, 
                        const std::string& msg = "") {
    ASSERT_EQ(actual.size(), expected.size()) 
        << msg << " size mismatch: expected " << expected.size() 
        << " bytes, got " << actual.size();
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(actual[i], expected[i]) 
            << msg << " byte mismatch at index " << i 
            << ": expected 0x" << std::hex << static_cast<int>(expected[i])
            << ", got 0x" << static_cast<int>(actual[i]);
    }
}

/**
 * @brief Create a hex dump string for debugging
 */
inline std::string hexDump(const std::vector<uint8_t>& data, size_t maxBytes = 64) {
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    size_t count = std::min(data.size(), maxBytes);
    for (size_t i = 0; i < count; ++i) {
        if (i > 0 && i % 16 == 0) oss << "\n";
        else if (i > 0) oss << " ";
        oss << std::setw(2) << static_cast<int>(data[i]);
    }
    if (data.size() > maxBytes) {
        oss << "... (" << (data.size() - maxBytes) << " more bytes)";
    }
    return oss.str();
}

/**
 * @brief Extract a range of bytes from a vector
 */
inline std::vector<uint8_t> extractBytes(const std::vector<uint8_t>& data, 
                                         size_t start, size_t length) {
    if (start >= data.size()) return {};
    size_t end = std::min(start + length, data.size());
    return std::vector<uint8_t>(data.begin() + start, data.begin() + end);
}

/**
 * @brief Read a 16-bit little-endian word from a byte vector
 */
inline uint16_t readWord(const std::vector<uint8_t>& data, size_t offset) {
    if (offset + 1 >= data.size()) return 0;
    return static_cast<uint16_t>(data[offset]) | 
           (static_cast<uint16_t>(data[offset + 1]) << 8);
}

// ============================================================================
// O65 HEADER CONSTANTS FOR TESTING
// ============================================================================
namespace TestConstants {
    constexpr uint8_t O65_MARKER_0 = 0x01;
    constexpr uint8_t O65_MARKER_1 = 0x00;
    constexpr uint8_t O65_MAGIC_O = 0x6F;  // 'o'
    constexpr uint8_t O65_MAGIC_6 = 0x36;  // '6'
    constexpr uint8_t O65_MAGIC_5 = 0x35;  // '5'
    constexpr uint8_t O65_VERSION = 0x00;
    constexpr size_t O65_HEADER_SIZE_16 = 26;
}

// ============================================================================
// BASE TEST FIXTURE
// ============================================================================

class CodegenTestBase : public ::testing::Test {
protected:
    CodegenVisitor visitor;

    void SetUp() override {
        // Visitor is freshly initialized for each test
    }

    // --- AST Factory Helpers ---

    std::unique_ptr<NumberNode> makeNumber(int32_t value) {
        return std::make_unique<NumberNode>(value);
    }

    std::unique_ptr<StringNode> makeString(std::string_view str) {
        return std::make_unique<StringNode>(str);
    }

    std::unique_ptr<BoolNode> makeBool(bool value) {
        return std::make_unique<BoolNode>(value);
    }

    std::unique_ptr<VariableNode> makeVariable(std::string_view name, 
                                               AstType type, 
                                               std::unique_ptr<Node> value = nullptr, 
                                               bool constant = false, 
                                               bool zeropage = false) {
        return std::make_unique<VariableNode>(name, type, std::move(value), constant, zeropage);
    }

    std::unique_ptr<ReferenceNode> makeRef(std::string_view name) {
        return std::make_unique<ReferenceNode>(name);
    }

    std::unique_ptr<AssignmentNode> makeAssign(std::unique_ptr<Node> assignee, 
                                               std::unique_ptr<Node> expr) {
        return std::make_unique<AssignmentNode>(std::move(assignee), std::move(expr));
    }

    std::unique_ptr<BinaryOperationNode> makeBinary(Operator op, 
                                                    std::unique_ptr<Node> left, 
                                                    std::unique_ptr<Node> right) {
        return std::make_unique<BinaryOperationNode>(op, std::move(left), std::move(right));
    }

    std::unique_ptr<UnaryNode> makeUnary(UnaryOperator op, std::unique_ptr<Node> expr) {
        return std::make_unique<UnaryNode>(op, std::move(expr));
    }

    std::unique_ptr<ComparisonNode> makeComp(Comparison comp, 
                                             std::unique_ptr<Node> left, 
                                             std::unique_ptr<Node> right) {
        return std::make_unique<ComparisonNode>(comp, std::move(left), std::move(right));
    }

    std::unique_ptr<IfNode> makeIf(std::unique_ptr<Node> cond, 
                                   std::vector<std::unique_ptr<Node>> body, 
                                   std::vector<std::unique_ptr<Node>> elseBody = {}) {
        return std::make_unique<IfNode>(std::move(cond), std::move(body), std::move(elseBody));
    }

    std::unique_ptr<WhileNode> makeWhile(std::unique_ptr<Node> cond, 
                                         std::vector<std::unique_ptr<Node>> body) {
        return std::make_unique<WhileNode>(std::move(cond), std::move(body));
    }

    std::unique_ptr<FunctionNode> makeFunction(std::string_view name, 
                                               std::map<std::string_view, AstType> params, 
                                               std::vector<std::unique_ptr<Node>> body, 
                                               bool interrupt = false) {
        return std::make_unique<FunctionNode>(name, std::move(params), std::move(body), interrupt);
    }

    std::unique_ptr<FunctionNode> makeFunctionDecl(std::string_view name, 
                                                   std::map<std::string_view, AstType> params, 
                                                   bool interrupt = false) {
        return std::make_unique<FunctionNode>(name, std::move(params), interrupt);
    }

    std::unique_ptr<CallNode> makeCall(std::string_view name, 
                                       std::vector<std::unique_ptr<Node>> params = {}) {
        return std::make_unique<CallNode>(name, std::move(params));
    }

    std::unique_ptr<StructureNode> makeStruct(std::string_view name, 
                                              std::map<std::string_view, AstType> members) {
        return std::make_unique<StructureNode>(name, std::move(members));
    }

    std::unique_ptr<MemberReferenceNode> makeMemberRef(std::string_view base, 
                                                       std::string_view member) {
        return std::make_unique<MemberReferenceNode>(base, member);
    }

    std::unique_ptr<IncludeNode> makeInclude(std::string_view path) {
        return std::make_unique<IncludeNode>(makeString(path));
    }

    std::unique_ptr<DefineNode> makeDefine(std::string_view name, 
                                           std::unique_ptr<Node> expr) {
        return std::make_unique<DefineNode>(name, std::move(expr));
    }

    // --- Accessors for Private Members ---

    const std::vector<uint8_t>& getTextSegment() const { 
        return visitor.textSegment; 
    }
    
    const std::vector<uint8_t>& getDataSegment() const { 
        return visitor.dataSegment; 
    }
    
    const std::vector<o65::O65_Relocation_Entry>& getTextRelocs() const { 
        return visitor.textRelocs; 
    }
    
    const std::vector<o65::O65_Relocation_Entry>& getDataRelocs() const { 
        return visitor.dataRelocs; 
    }
    
    const std::vector<Scope>& getScopes() const { 
        return visitor.scopes; 
    }
    
    size_t getEvalStackSize() const { 
        return visitor.evalStack.size(); 
    }
    
    EvaluationResult peekEvalStack() const {
        if (visitor.evalStack.empty()) {
            throw std::runtime_error("Eval stack is empty");
        }
        return visitor.evalStack.top();
    }
    
    EvaluationResult popEvalStack() {
        if (visitor.evalStack.empty()) {
            throw std::runtime_error("Eval stack is empty");
        }
        auto result = visitor.evalStack.top();
        visitor.evalStack.pop();
        return result;
    }
    
    std::optional<SymbolInfo> findSymbol(const std::string& name) {
        return visitor.findSymbol(name);
    }

    ZeroPageAllocator& getZeroPageAllocator() {
        return visitor.zeroPageAllocator;
    }

    const std::map<std::string, StructInfo>& getStructDefinitions() const {
        return visitor.structDefinitions;
    }

    const std::vector<Label>& getLabels() const {
        return visitor.labels;
    }

    void callLoadIntoRegister(const EvaluationResult& source) {
        visitor.loadIntoRegister(source);
    }

    // --- Verification Helpers ---

    void verifyO65Header(const std::vector<uint8_t>& binary) {
        ASSERT_GE(binary.size(), TestConstants::O65_HEADER_SIZE_16) 
            << "Binary too small for O65 header";
        
        EXPECT_EQ(binary[0], TestConstants::O65_MARKER_0) << "Invalid O65 marker byte 0";
        EXPECT_EQ(binary[1], TestConstants::O65_MARKER_1) << "Invalid O65 marker byte 1";
        EXPECT_EQ(binary[2], TestConstants::O65_MAGIC_O) << "Invalid O65 magic 'o'";
        EXPECT_EQ(binary[3], TestConstants::O65_MAGIC_6) << "Invalid O65 magic '6'";
        EXPECT_EQ(binary[4], TestConstants::O65_MAGIC_5) << "Invalid O65 magic '5'";
        EXPECT_EQ(binary[5], TestConstants::O65_VERSION) << "Invalid O65 version";
    }

    void expectTextContains(const std::vector<uint8_t>& expected, size_t startOffset = 0) {
        const auto& text = getTextSegment();
        ASSERT_GE(text.size(), startOffset + expected.size()) 
            << "Text segment too small to contain expected bytes at offset " << startOffset;
        
        for (size_t i = 0; i < expected.size(); ++i) {
            EXPECT_EQ(text[startOffset + i], expected[i]) 
                << "Text segment mismatch at offset " << (startOffset + i)
                << ": expected 0x" << std::hex << static_cast<int>(expected[i])
                << ", got 0x" << static_cast<int>(text[startOffset + i]);
        }
    }

    void expectDataContains(const std::vector<uint8_t>& expected, size_t startOffset = 0) {
        const auto& data = getDataSegment();
        ASSERT_GE(data.size(), startOffset + expected.size()) 
            << "Data segment too small to contain expected bytes at offset " << startOffset;
        
        for (size_t i = 0; i < expected.size(); ++i) {
            EXPECT_EQ(data[startOffset + i], expected[i]) 
                << "Data segment mismatch at offset " << (startOffset + i)
                << ": expected 0x" << std::hex << static_cast<int>(expected[i])
                << ", got 0x" << static_cast<int>(data[startOffset + i]);
        }
    }
};

// ============================================================================
// SPECIALIZED TEST FIXTURES
// ============================================================================

class CodegenSegmentTest : public CodegenTestBase {
protected:
    size_t getTextSegmentSize() const {
        return getTextSegment().size();
    }
    
    size_t getDataSegmentSize() const {
        return getDataSegment().size();
    }
};

class CodegenRelocationTest : public CodegenTestBase {
protected:
    bool hasTextRelocAt(uint32_t absoluteOffset) const {
        for (const auto& reloc : getTextRelocs()) {
            if (reloc.absolute_offset == absoluteOffset) {
                return true;
            }
        }
        return false;
    }
    
    bool hasDataRelocAt(uint32_t absoluteOffset) const {
        for (const auto& reloc : getDataRelocs()) {
            if (reloc.absolute_offset == absoluteOffset) {
                return true;
            }
        }
        return false;
    }
};

class ZeroPageAllocatorTest : public ::testing::Test {
protected:
    ZeroPageAllocator allocator;
};

class CodegenO65Test : public CodegenTestBase {
protected:
    std::vector<uint8_t> generateBinary() {
        return visitor.generateO65();
    }
    
    uint16_t extractModeWord(const std::vector<uint8_t>& binary) {
        if (binary.size() < 8) return 0;
        return readWord(binary, 6);
    }
    
    uint16_t extractTextLen(const std::vector<uint8_t>& binary) {
        if (binary.size() < 12) return 0;
        return readWord(binary, 10);
    }
    
    uint16_t extractDataLen(const std::vector<uint8_t>& binary) {
        if (binary.size() < 16) return 0;
        return readWord(binary, 14);
    }
};

class CodegenIntegrationTest : public CodegenTestBase {};
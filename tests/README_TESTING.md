# Testing Guide for Nixie Codegen

This document provides comprehensive documentation for the `CodegenVisitor` unit test suite.

## Table of Contents

1. [Running Tests](#running-tests)
2. [Test Organization](#test-organization)
3. [Adding New Tests](#adding-new-tests)
4. [Naming Conventions](#naming-conventions)
5. [Common Pitfalls](#common-pitfalls)
6. [Accessing Internal State](#accessing-internal-state)
7. [Binary Verification Tools](#binary-verification-tools)

---

## Running Tests

### Prerequisites

- CMake 3.14 or higher
- C++20 compatible compiler (Clang recommended)
- Google Test (automatically fetched by CMake)

### Build and Run

```bash
# From project root
mkdir -p build && cd build
cmake ..
cmake --build .

# Run all tests
ctest --output-on-failure

# Run specific test executable
./bin/compiler_tests

# Run with verbose output
ctest -V

# Run tests matching a pattern
./bin/compiler_tests --gtest_filter="*NumberNode*"
```

### Running Individual Test Suites

```bash
# Run only O65 format tests
./bin/compiler_tests --gtest_filter="CodegenO65Test.*"

# Run only integration tests
./bin/compiler_tests --gtest_filter="CodegenIntegrationTest.*"

# Run only zero-page allocator tests
./bin/compiler_tests --gtest_filter="ZeroPageAllocatorTest.*"

# Run only segment tests
./bin/compiler_tests --gtest_filter="*Segment*"
```

---

## Test Organization

### File Structure

| File | Purpose |
|------|---------|
| `test_helpers.hpp` | Shared fixtures, utilities, and accessors |
| `codegen_tests.cpp` | Individual visitor method tests |
| `codegen_segments_tests.cpp` | Text/data segment and relocation tests |
| `codegen_zeropage_tests.cpp` | ZeroPageAllocator unit tests |
| `codegen_o65_tests.cpp` | O65 binary format validation |
| `codegen_integration_tests.cpp` | End-to-end compilation scenarios |

### Test Fixtures

| Fixture | Base | Purpose |
|---------|------|---------|
| `CodegenTestBase` | `::testing::Test` | Primary fixture with factory helpers |
| `CodegenSegmentTest` | `CodegenTestBase` | Segment emission tests |
| `CodegenRelocationTest` | `CodegenTestBase` | Relocation table tests |
| `ZeroPageAllocatorTest` | `::testing::Test` | Standalone ZP allocator tests |
| `CodegenO65Test` | `CodegenTestBase` | O65 binary format tests |
| `CodegenIntegrationTest` | `CodegenTestBase` | Full program tests |

---

## Adding New Tests

### Step 1: Choose the Right File

- **Single visitor method behavior** → `codegen_tests.cpp`
- **Segment/relocation behavior** → `codegen_segments_tests.cpp`
- **ZeroPageAllocator logic** → `codegen_zeropage_tests.cpp`
- **O65 format validation** → `codegen_o65_tests.cpp`
- **Multi-feature/realistic programs** → `codegen_integration_tests.cpp`

### Step 2: Choose the Right Fixture

```cpp
// For most codegen tests
TEST_F(CodegenTestBase, YourTest) { ... }

// For O65 format tests
TEST_F(CodegenO65Test, YourTest) { ... }

// For standalone ZP allocator
TEST_F(ZeroPageAllocatorTest, YourTest) { ... }
```

### Step 3: Use Factory Helpers

```cpp
TEST_F(CodegenTestBase, Example_NewTest) {
    // Create AST nodes using helpers
    auto num = makeNumber(42);
    auto str = makeString("hello");
    auto var = makeVariable("x", AstType::Primitive(TypeKind::UNSIGNED_8));
    auto ref = makeRef("x");
    auto assign = makeAssign("x", makeNumber(10));
    
    // Create function with body
    std::map<std::string_view, AstType> params;
    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeNumber(1));
    auto func = makeFunction("test", params, std::move(body));
    
    // Visit nodes
    visitor.visit(*var);
    visitor.visit(*func);
    
    // Access internal state
    const auto& text = getTextSegment();
    const auto& data = getDataSegment();
    const auto& relocs = getTextRelocs();
    
    // Make assertions
    EXPECT_GT(text.size(), 0u);
}
```

### Step 4: Complete Assertion

Every test must have at least one assertion:

```cpp
// Good - complete test
TEST_F(CodegenTestBase, SomeFeature_Works) {
    auto node = makeNumber(5);
    visitor.visit(*node);
    EXPECT_EQ(getTextSegment().size(), 2u);  // LDA #$05
}

// Bad - incomplete test (NEVER DO THIS)
TEST_F(CodegenTestBase, SomeFeature_Works) {
    auto node = makeNumber(5);
    visitor.visit(*node);
    // TODO: add assertions  <- FORBIDDEN
}
```

---

## Naming Conventions

### Test Names

Follow the pattern: `Category_SpecificBehavior_Condition`

```cpp
TEST_F(CodegenTestBase, NumberNode_8BitValue_EmitsLDA)
TEST_F(CodegenTestBase, VariableNode_ZeroPage_AllocatesCorrectly)
TEST_F(CodegenTestBase, IfNode_WithElse_HasJumpToEnd)
TEST_F(CodegenO65Test, Header_ModeField_CPU6502)
```

### Categories by Node Type

- `NumberNode_*` - Numeric literal tests
- `StringNode_*` - String literal tests
- `BoolNode_*` - Boolean literal tests
- `VariableNode_*` - Variable declaration tests
- `ReferenceNode_*` - Variable reference tests
- `AssignmentNode_*` - Assignment tests
- `BinaryOperationNode_*` - Binary operation tests
- `UnaryNode_*` - Unary operation tests
- `ComparisonNode_*` - Comparison tests
- `IfNode_*` - If statement tests
- `WhileNode_*` - While loop tests
- `FunctionNode_*` - Function definition tests
- `StructureNode_*` - Struct definition tests
- `CallNode_*` - Function call tests
- `MemberReferenceNode_*` - Member access tests

---

## Common Pitfalls

### 1. Forgetting to Visit Nodes

```cpp
// Wrong - node created but not visited
auto var = makeVariable("x", ...);
EXPECT_GT(getDataSegment().size(), 0u);  // Fails!

// Correct
auto var = makeVariable("x", ...);
visitor.visit(*var);  // Don't forget this!
EXPECT_GT(getDataSegment().size(), 0u);
```

### 2. Using Raw Pointers Instead of unique_ptr

```cpp
// Wrong - will leak memory
Node* node = new NumberNode(5);

// Correct - use factory helpers
auto node = makeNumber(5);
```

### 3. Moving Objects Twice

```cpp
// Wrong - use after move
auto node = makeNumber(5);
body.push_back(std::move(node));
visitor.visit(*node);  // CRASH - node was moved!

// Correct
auto node = makeNumber(5);
visitor.visit(*node);  // Visit first
body.push_back(std::move(node));  // Then move
```

### 4. Incorrect Symbol Lookup Order

```cpp
// Wrong - reference before declaration
auto ref = makeRef("x");
visitor.visit(*ref);  // Error: undefined symbol

auto var = makeVariable("x", ...);
visitor.visit(*var);

// Correct - declare before use
auto var = makeVariable("x", ...);
visitor.visit(*var);

auto ref = makeRef("x");
visitor.visit(*ref);  // Works!
```

### 5. Expecting Code in Data Segment

```cpp
// Wrong - functions emit to text, not data
auto func = makeFunction(...);
visitor.visit(*func);
EXPECT_GT(getDataSegment().size(), 0u);  // Fails!

// Correct
EXPECT_GT(getTextSegment().size(), 0u);  // Functions go to text
```

---

## Accessing Internal State

The `CodegenTestBase` fixture provides accessor methods for private members:

### Segment Access

```cpp
// Get raw segment data
const std::vector<uint8_t>& text = getTextSegment();
const std::vector<uint8_t>& data = getDataSegment();

// Get relocation tables
const std::vector<o65::O65_Relocation_Entry>& textRelocs = getTextRelocs();
const std::vector<o65::O65_Relocation_Entry>& dataRelocs = getDataRelocs();
```

### Symbol Table Access

```cpp
// Look up symbols
const SymbolInfo* sym = findSymbol("variableName");
if (sym) {
    // sym->storageClass, sym->offset, sym->type, etc.
}
```

### ZeroPage Allocator Access

```cpp
ZeroPageAllocator& zp = getZeroPage();
size_t available = zp.available();
size_t largest = zp.largestAvailableBlock();
```

### Label Access

```cpp
const std::map<std::string, Label>& labels = getLabels();
```

### Binary Generation

```cpp
std::vector<uint8_t> binary = generateBinary();
```

---

## Binary Verification Tools

### Utility Functions

```cpp
// Verify exact byte sequence at offset
expectBytes(segment, expectedBytes, "description");

// Hex dump for debugging
std::string hex = hexDump(segment, start, length);
std::cout << hex << std::endl;

// Extract bytes from segment
std::vector<uint8_t> bytes = extractBytes(segment, offset, length);

// Read 16-bit word (little-endian)
uint16_t word = readWord(segment, offset);
```

### O65 Header Helpers

```cpp
// Verify valid O65 header
verifyO65Header(binary);

// Extract specific header fields
uint16_t mode = extractModeWord(binary);
uint16_t textLen = extractTextLen(binary);
uint16_t dataLen = extractDataLen(binary);
```

### Example: Debugging Failed Test

```cpp
TEST_F(CodegenTestBase, Debug_Example) {
    auto node = makeNumber(0x42);
    visitor.visit(*node);
    
    const auto& text = getTextSegment();
    
    // Print hex dump for debugging
    std::cout << "Text segment:\n" << hexDump(text, 0, text.size()) << std::endl;
    
    // Verify expected bytes
    std::vector<uint8_t> expected = {
        static_cast<uint8_t>(Opcodes::LDA_IMM), 0x42
    };
    expectBytes(text, expected, "LDA #$42");
}
```

### Test Constants

```cpp
namespace TestConstants {
    constexpr size_t O65_HEADER_SIZE_16 = 26;
    constexpr uint8_t O65_MAGIC_1 = 0x01;
    constexpr uint8_t O65_MAGIC_2 = 0x00;
    constexpr uint8_t O65_MAGIC_O = 0x6F;
    constexpr uint8_t O65_MAGIC_6 = 0x36;
    constexpr uint8_t O65_MAGIC_5 = 0x35;
}
```

---

## Test Coverage Goals

### Visitor Methods (18 total)

- [x] `visit(NumberNode&)`
- [x] `visit(StringNode&)`
- [x] `visit(BoolNode&)`
- [x] `visit(VariableNode&)`
- [x] `visit(ReferenceNode&)`
- [x] `visit(AssignmentNode&)`
- [x] `visit(BinaryOperationNode&)`
- [x] `visit(UnaryNode&)`
- [x] `visit(ComparisonNode&)`
- [x] `visit(IfNode&)`
- [x] `visit(WhileNode&)`
- [x] `visit(FunctionNode&)`
- [x] `visit(StructureNode&)`
- [x] `visit(StructureInitNode&)`
- [x] `visit(CallNode&)`
- [x] `visit(MemberReferenceNode&)`
- [x] `visit(IncludeNode&)` (no-op)
- [x] `visit(DefineNode&)` (no-op)

### Additional Coverage

- [x] Zero-page allocation/deallocation
- [x] Text segment emission
- [x] Data segment emission  
- [x] Relocation table generation
- [x] O65 binary format
- [x] Integration scenarios

---

## Troubleshooting

### Test Won't Compile

1. Check `#include "test_helpers.hpp"` is present
2. Verify fixture inheritance matches test macro (TEST_F vs TEST)
3. Ensure all factory helpers have correct argument types

### Segmentation Fault

1. Check for use-after-move on unique_ptr
2. Verify nodes are visited before being moved
3. Check symbol references exist before use

### Assertion Failures

1. Use `hexDump()` to inspect actual segment contents
2. Check opcode values in `opcodes.h`
3. Verify node creation parameters are correct

### Linker Errors

1. Ensure `codegen.cpp` is linked with tests
2. Verify all required source files are in CMakeLists.txt

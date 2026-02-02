
#pragma once

#include "lexer.hpp"
#include "ast.hpp"
#include "o65.hpp"
#include "opcodes.h"
#include <vector>
#include <map>
#include <string>
#include <cstdint>
#include <stack>
#include <array>
#include <variant>
#include <optional>
#include <stdexcept>
#include <algorithm>

using namespace o65;

class ZeroPageAllocator {
private:
    // Track which zero page addresses are in use
    std::array<bool, 256> allocated;
    
    // Reserve system areas (6502 convention)
    static constexpr uint8_t SYSTEM_START = 0x00;
    static constexpr uint8_t SYSTEM_END = 0x08;    // Stack pointer area
    static constexpr uint8_t USER_START = 0x08;
    static constexpr uint8_t USER_END = 0xFF;
    
    uint8_t nextFreeAddress;

public:
    ZeroPageAllocator() : allocated{}, nextFreeAddress(USER_START) {
        // Mark system area as allocated
        for (uint8_t i = SYSTEM_START; i < SYSTEM_END; ++i) {
            allocated[i] = true;
        }
    }
    
    /**
     * Allocate contiguous zero page bytes
     * @param size Number of bytes to allocate
     * @return Starting address, or nullopt if allocation fails
     */
    std::optional<uint8_t> allocate(size_t size) {
        if (size == 0 || size > 256) {
            return std::nullopt;
        }
        
        // Use uint16_t to avoid overflow when start reaches 255
        uint16_t endLimit = static_cast<uint16_t>(USER_END) - size + 1;
        
        // Try to find contiguous free space
        for (uint16_t start = USER_START; start <= endLimit; ++start) {
            bool canAllocate = true;
            
            // Check if all needed bytes are free
            for (size_t i = 0; i < size; ++i) {
                if (allocated[start + i]) {
                    canAllocate = false;
                    break;
                }
            }
            
            if (canAllocate) {
                // Mark as allocated
                for (size_t i = 0; i < size; ++i) {
                    allocated[start + i] = true;
                }
                
                // Update next free hint
                nextFreeAddress = static_cast<uint8_t>(start + size);
                
                return static_cast<uint8_t>(start);
            }
        }
        
        return std::nullopt; // No space available
    }
    
    /**
     * Free previously allocated zero page bytes
     */
    void free(uint8_t address, size_t size) {
        for (size_t i = 0; i < size && (address + i) < 256; ++i) {
            allocated[address + i] = false;
        }
        
        // Update hint if we freed earlier memory
        if (address < nextFreeAddress) {
            nextFreeAddress = address;
        }
    }
    
    /**
     * Reserve a specific zero page address (for hardware registers, etc.)
     */
    bool reserve(uint8_t address, size_t size = 1) {
        if (address + size > 256) {
            return false;
        }
        
        for (size_t i = 0; i < size; ++i) {
            if (allocated[address + i]) {
                return false; // Already in use
            }
            allocated[address + i] = true;
        }
        
        return true;
    }
    
    /**
     * Check how much zero page space is available
     */
    size_t available() const {
        size_t count = 0;
        for (size_t i = USER_START; i <= USER_END; ++i) {
            if (!allocated[i]) {
                ++count;
            }
        }
        return count;
    }
    
    /**
     * Get largest contiguous block available
     */
    size_t largestBlock() const {
        size_t maxBlock = 0;
        size_t currentBlock = 0;
        
        for (size_t i = USER_START; i <= USER_END; ++i) {
            if (!allocated[i]) {
                ++currentBlock;
                maxBlock = std::max(maxBlock, currentBlock);
            } else {
                currentBlock = 0;
            }
        }
        
        return maxBlock;
    }

    size_t totalAllocated() const {
        size_t count = 0;
        for (size_t i = 0; i < 256; ++i) {
            if (allocated[i]) {
                ++count;
            }
        }
        return count;
    }
};

// Represents where a variable lives
enum class StorageClass {
    Register,   // A, X, or Y (Optimization, not used in simple pass)
    Stack,      // Relative to Stack Pointer (Local variables)
    Global,     // Absolute address in DATA/BSS segment
    ZeroPage    // 6502 Zero Page
};

// Information about a defined symbol (variable or function)
struct SymbolInfo {
    std::string name;
    AstType type;
    StorageClass storage;
    int32_t offset; // Stack offset (negative) or Segment offset (positive)
    bool isConstant;
    bool isExternal = false; // Imported symbol
    size_t size;    // Size in bytes
};

// Information about a defined Structure type
struct StructInfo {
    std::string name;
    size_t totalSize;
    // Map member name to pair<offset, type>
    std::map<std::string, std::pair<size_t, AstType>> members; 
};

// Context for the current scope (function body, block)
struct Scope {
    std::map<std::string, SymbolInfo> symbols;
    size_t stackFrameSize = 0; // Bytes allocated in this scope
};

// Label for jump targets (If/While/Forward calls)
struct Label {
    size_t id;
    std::string name;
    std::vector<size_t> references; // Offsets in textSegment that point here
    std::optional<size_t> location; // Resolved offset in textSegment
};

enum class ValueLocation {
    Accumulator,   // Value is in A register
    DataSegment,   // Value is at data segment offset (for strings/constants)
    Immediate,      // Value is a compile-time constant
    DereferencedPointer // Value is at address pointed to by a pointer
};


struct EvaluationResult {
    ValueLocation location;
    uint32_t value;  // Offset (for DataSegment) or immediate value
    AstType type;
};

struct ZeroPageAllocation {
    std::string symbolName;
    uint8_t address;
    size_t size;
};

class CodegenVisitor : public Visitor
{
private:

    friend class CodegenTestBase;
    
    // --- Segment Buffers ---
    std::vector<uint8_t> textSegment; // Executable Code
    std::vector<uint8_t> dataSegment; // Initialized Global Data
    
    // --- O65 Specifics ---
    
    std::vector<O65_Relocation_Entry> textRelocs;
    std::vector<O65_Relocation_Entry> dataRelocs;
    std::vector<std::string> undefinedList; // External imports
    std::vector<std::pair<std::string, size_t>> exportedGlobals; // Exports

    // --- Interrupt Vector Table ---
    std::optional<std::string> nmiHandler;
    std::optional<std::string> resetHandler;
    std::optional<std::string> irqHandler;

    // --- Compilation State ---
    std::vector<Scope> scopes;      // Stack of scopes
    std::map<std::string, StructInfo> structDefinitions; // Struct registry
    std::vector<Label> labels;      // Control flow labels
    

    std::stack<EvaluationResult> evalStack;

    // Current function context
    std::optional<SymbolInfo> currentFunction;
    int currentStackDepth = 0; // Tracks generic stack usage (pushes/pops)
    bool currentFunctionIsInterrupt = false; // For proper epilogue generation

    ZeroPageAllocator zeroPageAllocator;
    
    // Track zero page allocations for cleanup
    std::vector<ZeroPageAllocation> zeroPageAllocations;

    constexpr std::optional<ZeroPageAllocation> getZeroPageAllocation(std::string name) {
        for (auto zp : zeroPageAllocations) {
            if (zp.symbolName == name)
                return zp;
        }
        return std::nullopt;
    }
    // --- Emitters ---
    void emit(uint8_t byte);
    void emitOp(uint8_t opcode) { emit(opcode); };
    void emitWord(uint16_t word);
    void emitData(uint8_t byte);
    void emitDataWord(uint16_t word);
    
    // --- Relocation Helpers ---
    // Mark the last emitted word/byte as needing relocation
    void addTextReloc(o65::RelocationType type, o65::SegmentID target);
    void addDataReloc(o65::RelocationType type, o65::SegmentID target);

    // --- Scope & Symbol Management ---
    void enterScope();
    void exitScope();
    void declareVariable(const std::string& name, AstType type, StorageClass storage, size_t size);
    std::optional<SymbolInfo> findSymbol(const std::string& name);
    size_t getSizeOfType(const AstType& type);

    // --- Label Management ---
    size_t createLabel(const std::string& debugName = "");
    void referenceLabel(size_t labelId, int offset = 0);
    void emitLabel(size_t labelId); // Mark current location
    void emitJump(uint8_t opcode, size_t labelId); // JMP/JSR/Branch to label
    
    // --- Internal Logic ---
    void loadVariableToRegister(const SymbolInfo& sym, const std::string& member = "");
    void saveRegisterToVariable(const SymbolInfo& sym, const std::string& member = "");

    // void loadNumberToRegister(const EvaluationResult &numResult);
    void storeNumberToMemory(uint32_t value, size_t offset, StorageClass storage, TypeKind kind);

    inline size_t getOrCreateUndefinedReference(const std::string& name) {
        // Check if already in list
        auto it = std::find(undefinedList.begin(), undefinedList.end(), name);
        if (it != undefinedList.end()) {
            return std::distance(undefinedList.begin(), it);
        }
        
        // Add new undefined reference
        undefinedList.push_back(name);
        return undefinedList.size() - 1;
    }

    inline StructInfo& getStructDefinition(const AstType& type) {
        if (type.kind != TypeKind::STRUCTURE) {
            throw std::runtime_error("Member access on non-struct type");
        }
        
        auto structType = std::string(type.name);
        auto it = structDefinitions.find(structType);
        if (it == structDefinitions.end()) {
            throw std::runtime_error("Unknown struct: " + structType);
        }
        return it->second;
    }

    inline constexpr o65::O65_Mode getMode() const {
        o65::O65_Mode res{};
        res.processor = o65::ProcessorVariant::CPU_6502;
        res.granularity = o65::RelocationGranularity::BYTEWISE;
        res.address_size = o65::AddressSize::BITS_16;
        res.file_type = o65::FileType::OBJECT_FILE;
        res.simple_mode = o65::SimpleAddressing::NORMAL;
        res.chain = o65::ChainFlag::STANDALONE;
        res.bss_zero = o65::BssZeroFlag::MUST_ZERO;
        res.cpu2_type = o65::CPU2Type::CPU2_65C02; // current necessity
        res.alignment = o65::AlignmentMode::ALIGN_BYTE;
        return res;
    }

    const std::vector<uint8_t> writeUndefinedReferences();
    const std::vector<uint8_t> writeRelocationTable(const std::vector<o65::O65_Relocation_Entry> relocations, const uint32_t base);
    const std::vector<uint8_t> writeExportedGlobals();

    const std::vector<uint8_t> buildHeaderOptions();

    // Emit function entry sequence (stack frame setup)
    void emitPrologue(size_t frameSize);
    // Purpose: Pushes old FP, moves SP to FP, reserves stack space.
    // Called when: visit(FunctionNode) enters.

    // Emit function exit sequence
    void emitEpilogue(size_t frameSize);
    // Purpose: Restores SP, pops old FP, RTS.
    // Called when: visit(FunctionNode) exits.

    // Move a value from a generic location (stack/mem/imm) into CPU registers
    void loadIntoRegister(const EvaluationResult& source);
    // Purpose: Ensures the value is in A (8-bit) or A/X (16-bit) for processing.
    // Called when: Preparing operands for BinaryOp/Call/Assignment.

    // Push a register value onto the CPU hardware stack
    void pushRegister(const AstType& type);
    // Purpose: Emits PHA (and PHX/PHY) depending on type size.
    // Called when: Preserving temporary results in BinaryOp.

    // Pop a value from CPU hardware stack into registers
    void popRegister(const AstType& type);
    // Purpose: Emits PLA (and PLX/PLY) depending on type size.
    // Called when: Restoring temporary results in BinaryOp.

    // Emit conditional branch with long-jump fallback
    void emitConditionalBranch(uint8_t opcode, size_t labelId);
    // Purpose: Emits BNE/BEQ. Checks distance. If >127 bytes, inverts logic and jumps.
    // Called when: visit(IfNode), visit(WhileNode).

    // Calculate effective address of a structure member
    uint32_t calculateMemberOffset(const AstType& structType, const std::string& memberName);
    // Purpose: Returns the byte offset of a member within a struct.
    // Called when: visit(MemberReferenceNode).
    
    // --- Multiplication and Division ---
    // Emit 8-bit multiply using shift-and-add algorithm
    // Input: A = multiplier, temp zeropage = multiplicand
    // Output: A = low byte, X = high byte
    void emitMultiply8Shift();
    
    // Emit 8-bit multiply using square lookup table
    // Uses formula: a*b = (sq[a+b] - sq[|a-b|]) where sq[n] = n²/4
    // Input: A = multiplier, temp zeropage = multiplicand
    // Output: A = low byte, X = high byte
    void emitMultiply8Table();
    
    // Emit 16-bit multiply using shift-and-add algorithm
    // Input: __temporary (2 bytes) = multiplier, __temporary_2p (2 bytes) = multiplicand
    // Output: Result stored in __temporary_2p (4 bytes for full 32-bit result)
    void emitMultiply16Shift();
    
    // Emit 8-bit divide using repeated subtraction
    // Input: A = dividend, temp zeropage = divisor
    // Output: A = quotient, X = remainder
    void emitDivide8();
    
    // Emit 16-bit divide using repeated subtraction
    // Input: __temporary (2 bytes) = dividend, __temporary_2p (2 bytes) = divisor
    // Output: __temporary = quotient, __temporary_2p = remainder
    void emitDivide16();
    
    // Generate and embed the squares lookup table in data segment
    // Returns the offset in data segment where table starts
    size_t embedSquaresTable();
    
    // Track if squares table has been embedded
    bool squaresTableEmbedded = false;
    size_t squaresTableOffset = 0;

    // Load stack variable into accumulator
    void loadStackVariable(int32_t offset, size_t size);
    
    // Store accumulator to stack variable
    void storeStackVariable(int32_t offset, size_t size);

public:
    CodegenVisitor() {
        // Initialize global scope
        enterScope();

        // Reserve zero page addresses for system use
        zeroPageAllocator.reserve(0x00, 8); // Stack pointer area
        zeroPageAllocations.push_back({"__stack_pointer", 0x00, 2});
        zeroPageAllocations.push_back({"__frame_pointer", 0x02, 2});
        zeroPageAllocations.push_back({"__temporary", 0x04, 1});
        zeroPageAllocations.push_back({"__temporary_dos", 0x05, 1});
        zeroPageAllocations.push_back({"__temporary_2p", 0x06, 2});

    }


    // Visitor Interface
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
    void visit(ReturnNode &node) override;


    // --- Finalization ---
    // Assembles the segments into a final O65 binary blob
    std::vector<uint8_t> generateO65();
    
    // Generate the 6502 vector table segment
    // std::vector<uint8_t> generateVectorTable();
    bool addVectorTable = false;
    implementation_defined::O65_File_Layout generateVectorTable();
};

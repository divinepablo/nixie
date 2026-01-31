#include "codegen.hpp"
#include <stdexcept>
void CodegenVisitor::emit(uint8_t byte)
{
    textSegment.push_back(byte);
}

void CodegenVisitor::emitWord(uint16_t word)
{
    emit(static_cast<uint8_t>(word & 0x00FF));
    emit(static_cast<uint8_t>((word & 0xFF00) >> 8));
}

void CodegenVisitor::emitData(uint8_t byte)
{
    dataSegment.push_back(byte);
}

void CodegenVisitor::emitDataWord(uint16_t word)
{
    emitData(static_cast<uint8_t>(word & 0x00FF));
    emitData(static_cast<uint8_t>((word & 0xFF00) >> 8));
}

void CodegenVisitor::addTextReloc(o65::RelocationType type, o65::SegmentID target)
{
    // We store the absolute offset within the text segment here.
    // The generateO65() function will convert this to the delta-encoded format required by the spec.
    O65_Relocation_Entry entry;
    entry.offset_from_previous = 0; // Calculated later
    entry.absolute_offset = static_cast<uint32_t>(textSegment.size());
    
    // For WORD/ADDR relocations, we point to the start of the address (2 bytes written previously)
    // For HIGH/LOW, we point to the byte just written.
    if (type == o65::RelocationType::WORD) {
        entry.absolute_offset -= 2;
    } else {
        entry.absolute_offset -= 1;
    }

    entry.typebyte = o65::make_typebyte(type, target);
    entry.symbol_index = 0; 
    entry.low_byte = 0;
    entry.seg_offset = 0;

    textRelocs.push_back(entry);
}

void CodegenVisitor::addDataReloc(o65::RelocationType type, o65::SegmentID target)
{
    O65_Relocation_Entry entry;
    entry.offset_from_previous = 0; // Calculated later
    entry.absolute_offset = static_cast<uint32_t>(dataSegment.size());

    if (type == o65::RelocationType::WORD) {
        entry.absolute_offset -= 2;
    } else {
        entry.absolute_offset -= 1;
    }

    entry.typebyte = o65::make_typebyte(type, target);
    entry.symbol_index = 0;
    entry.low_byte = 0;
    entry.seg_offset = 0;

    dataRelocs.push_back(entry);
}

void CodegenVisitor::enterScope()
{
    scopes.emplace_back();
    // Inherit the stack frame size from the parent scope conceptually, 
    // but usually, we track the frame relative to the function entry.
    // For this simple compiler, we reset frame size per block or keep it monotonic?
    // Let's copy the frame size from the previous scope to continue allocation.
    if (scopes.size() > 1) {
        scopes.back().stackFrameSize = scopes[scopes.size() - 2].stackFrameSize;
    }
}

void CodegenVisitor::exitScope()
{
    if (!scopes.empty()) {
        scopes.pop_back();
    }
}

void CodegenVisitor::declareVariable(const std::string& name, AstType type, StorageClass storage, size_t size)
{
    if (scopes.empty()) return;

    SymbolInfo info;
    info.name = name;
    info.type = type;
    info.storage = storage;
    info.size = size;
    info.isConstant = false; // Adjusted by visitor if needed

    Scope& currentScope = scopes.back();

    if (currentScope.symbols.find(name) != currentScope.symbols.end()) {
        throw std::runtime_error("Redeclaration of variable: " + name);
    }

    if (storage == StorageClass::Stack) {
        // Simple stack allocation: grow downwards or upwards.
        // 6502 cc65 convention often uses a zero-page 'sp' pointer.
        // Let's assign an offset relative to the base of the current frame.
        // We will increment the tracked size.
        info.offset = static_cast<int32_t>(currentScope.stackFrameSize);
        currentScope.stackFrameSize += size;
        
        // Align to 2 bytes if needed? 6502 doesn't strictly require it, avoiding padding.
    } else if (storage == StorageClass::Global) {
        // Offset into the Data segment
        info.offset = static_cast<int32_t>(dataSegment.size());
        
        // Reserve space in data segment (zero-init)
        for(size_t i = 0; i < size; ++i) {
            emitData(0);
        }
    }

    switch (storage) {
        case StorageClass::Stack:
            // Allocate on stack (negative offset from frame pointer)
            currentScope.stackFrameSize += size;
            info.offset = -static_cast<int32_t>(currentScope.stackFrameSize);
            break;
            
        case StorageClass::Global:
            // Allocate in DATA/BSS segment
        info.offset = static_cast<int32_t>(dataSegment.size());
            // Reserve space in data segment (zero-initialized)
            for (size_t i = 0; i < size; ++i) {
                emitData(0x00);
            }
            break;
            
        case StorageClass::ZeroPage: {
            info.offset = zeroPageAllocator.allocate(size).value(); // i hope this throws error
            break;
        }
            
        
            
            
        case StorageClass::Register:
            // Register allocation is an optimization - not used in simple pass
            info.offset = 0;
            break;
    }

    currentScope.symbols[name] = info;
}

std::optional<SymbolInfo> CodegenVisitor::findSymbol(const std::string& name)
{
    // Search from inner-most scope to outer-most
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->symbols.find(name);
        if (found != it->symbols.end()) {
            return found->second;
        }
    }
    return std::nullopt;
}

size_t CodegenVisitor::getSizeOfType(const AstType& type)
{
    if (type.isPointer()) return 2; // 16-bit pointers

    switch (type.kind) {
        case TypeKind::UNSIGNED_8:
        case TypeKind::SIGNED_8:
        case TypeKind::BOOLEAN:
            return 1;
        case TypeKind::UNSIGNED_16:
        case TypeKind::SIGNED_16:
            return 2;
        case TypeKind::UNSIGNED_32:
        case TypeKind::SIGNED_32:
            return 4;
        case TypeKind::UNSIGNED_64:
        case TypeKind::SIGNED_64:
            return 8;
        case TypeKind::STRING:
            return 2; // Pointer to string data
        case TypeKind::STRUCTURE: {
            std::string sName(type.name);
            if (structDefinitions.count(sName)) {
                return structDefinitions[sName].totalSize;
            }
            return 0; // Unknown struct
        }
        default:
            return 0;
    }
}

size_t CodegenVisitor::createLabel(const std::string &debugName)
{
    size_t id = labels.size();
    labels.push_back({id, debugName, {}, std::nullopt});
    return id;
}

void CodegenVisitor::referenceLabel(size_t labelId, int offset)
{
    if (labelId >= labels.size()) {
        throw std::runtime_error("Invalid label ID: " + std::to_string(labelId));
    }
    
    Label& label = labels[labelId];
    if (label.location.has_value()) {
        throw std::runtime_error("Label already defined: " + label.name);
    }

    label.references.push_back(textSegment.size() + offset);
}

void CodegenVisitor::emitLabel(size_t labelId)
{
    if (labelId >= labels.size()) {
        throw std::runtime_error("Invalid label ID: " + std::to_string(labelId));
    }
    
    Label& label = labels[labelId];
    if (label.location.has_value()) {
        throw std::runtime_error("Label already defined: " + label.name);
    }
    
    // Mark current text segment position as label location
    label.location = textSegment.size();
    
    // Patch all forward references
    for (size_t refOffset : label.references) {
        // Calculate relative offset for branch instructions
        // For 6502: 8-bit signed offset from instruction END
        int32_t relativeOffset = static_cast<int32_t>(*label.location) - 
                                 static_cast<int32_t>(refOffset + 1);
        
        if (relativeOffset < -128 || relativeOffset > 127) {
            throw std::runtime_error("Branch target too far for label: " + label.name);
        }
        
        // Patch the offset byte
        textSegment[refOffset] = static_cast<uint8_t>(relativeOffset & 0xFF);
    }
    
    label.references.clear();
}

void CodegenVisitor::emitJump(uint8_t opcode, size_t labelId)
{
    if (labelId >= labels.size()) {
        throw std::runtime_error("Invalid label ID: " + std::to_string(labelId));
    }
    
    Label& label = labels[labelId];
    emitOp(opcode);
    
    // Check if this is a branch instruction (relative) or absolute jump
    bool isBranch = (opcode != Opcodes::JMP_ABSOLUTE && opcode != Opcodes::JMP_ABSOLUTE_INDIRECT && opcode != Opcodes::JMP_ABSOLUTE_X_INDIRECT && opcode != Opcodes::JSR); // All 6502 branches
    
    if (isBranch) {
        // Branch instruction - 8-bit relative offset
        if (label.location.has_value()) {
            // Forward reference resolved
            int32_t relativeOffset = static_cast<int32_t>(*label.location) - 
                                     static_cast<int32_t>(textSegment.size() + 1);
            
            if (relativeOffset < -128 || relativeOffset > 127) {
                throw std::runtime_error("Branch target too far: " + label.name);
            }
            
            emit(static_cast<uint8_t>(relativeOffset & 0xFF));
        } else {
            // Forward reference - record position for later patching
            referenceLabel(labelId);
            emit(0x00); // Placeholder
        }
    } else {
        // Absolute jump (JMP/JSR) - 16-bit address
        if (label.location.has_value()) {
            emitWord(static_cast<uint16_t>(*label.location));
        } else {
            // Forward reference
            referenceLabel(labelId);
            emitWord(0x0000); // Placeholder
            
            // Add relocation entry for TEXT segment self-reference
            addTextReloc(RelocationType::WORD, SegmentID::TEXT);
        }
    }
}

void CodegenVisitor::loadVariableToRegister(const SymbolInfo &sym, const std::string &member)
{
    size_t offset = sym.offset;
    
    // Handle struct member access
    if (!member.empty()) {
        auto structType = std::string(sym.type.name);
        auto& structInfo = getStructDefinition(sym.type);
        
        auto memberIt = structInfo.members.find(member);
        if (memberIt == structInfo.members.end()) {
            throw std::runtime_error("Unknown member: " + member);
        }
        
        offset += memberIt->second.first; // Add member offset
    }
    
    switch (sym.storage) {
        case StorageClass::Global:
            // LDA absolute_address
            emitOp(Opcodes::LDA_ABSOLUTE); // LDA absolute
            emitWord(static_cast<uint16_t>(offset));
            // Mark for relocation
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
            break;
            
        case StorageClass::Stack:
            // Load from stack frame (6502 doesn't have native stack addressing)
            // Need to compute effective address and load
            // This is complex - simplified version:
            throw std::runtime_error("Stack variable access not yet implemented");
            break;
            
        case StorageClass::ZeroPage:
            // LDA zeropage
            emitOp(Opcodes::LDA_ZEROPAGE); // LDA zero page
            emit(static_cast<uint8_t>(offset & 0xFF));
            break;
            
        case StorageClass::Register:
            // Already in register - no-op
            break;
    }
}

void CodegenVisitor::saveRegisterToVariable(const SymbolInfo& sym, 
                                            const std::string& member) {
    size_t offset = sym.offset;
    
    // Handle struct member access (same as load)
    if (!member.empty()) {
        offset += calculateMemberOffset(sym.type, member);
    }
    
    switch (sym.storage) {
        case StorageClass::Global:
            // STA absolute_address
            emitOp(Opcodes::STA_ABSOLUTE); // STA absolute
            emitWord(static_cast<uint16_t>(offset));
            // Mark for relocation
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
            break;
            
        case StorageClass::Stack:
            throw std::runtime_error("Stack variable access not yet implemented");
            break;
            
        case StorageClass::ZeroPage:
            // STA zeropage
            emitOp(Opcodes::STA_ZEROPAGE); // STA zero page
            emit(static_cast<uint8_t>(offset & 0xFF));
            break;
            
        case StorageClass::Register:
            // Already in register - no-op
            break;
    }
}

/**
 * Helper to store a number from register(s) to memory
 * Used by assignment and other operations
 */
void CodegenVisitor::storeNumberToMemory(uint32_t value, size_t offset, 
                                         StorageClass storage, TypeKind kind) {
    switch (kind) {
        case TypeKind::UNSIGNED_8:
        case TypeKind::SIGNED_8:
        case TypeKind::BOOLEAN:
            // 8-bit store
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit(static_cast<uint8_t>(value & 0xFF));
            
            if (storage == StorageClass::Global) {
                emitOp(Opcodes::STA_ABSOLUTE); // 0x8D
                emitWord(static_cast<uint16_t>(offset));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
            } else if (storage == StorageClass::ZeroPage) {
                emitOp(Opcodes::STA_ZEROPAGE); // 0x85
                emit(static_cast<uint8_t>(offset & 0xFF));
            } else if (storage == StorageClass::Stack) {
                auto temp = getZeroPageAllocation("__temporary").value().address;
                auto temp2 = getZeroPageAllocation("__temporary_2p").value().address;
                auto fp = getZeroPageAllocation("__frame_pointer").value().address;
                
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::SEC);
                
                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(offset & 0xFF);
                
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp2);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp+1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(0x00);

                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp2 + 1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);

                emitOp(Opcodes::PHA);

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x01);
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);
                emitOp(Opcodes::TAX);

                emitOp(Opcodes::PLA);


                

            }
            break;
            
        case TypeKind::UNSIGNED_16:
        case TypeKind::SIGNED_16:
            // 16-bit store - low byte then high byte
            
            // Store low byte
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit(static_cast<uint8_t>(value & 0xFF));
            
            if (storage == StorageClass::Global) {
                emitOp(Opcodes::STA_ABSOLUTE);
                emitWord(static_cast<uint16_t>(offset));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
            } else if (storage == StorageClass::ZeroPage) {
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(static_cast<uint8_t>(offset & 0xFF));
            }
            
            // Store high byte
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit(static_cast<uint8_t>((value >> 8) & 0xFF));
            
            if (storage == StorageClass::Global) {
                emitOp(Opcodes::STA_ABSOLUTE);
                emitWord(static_cast<uint16_t>(offset + 1));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
            } else if (storage == StorageClass::ZeroPage) {
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(static_cast<uint8_t>((offset + 1) & 0xFF));
            }
            break;
            
        case TypeKind::UNSIGNED_32:
        case TypeKind::SIGNED_32:
            // 32-bit store - four bytes
            for (int i = 0; i < 4; ++i) {
                emitOp(Opcodes::LDA_IMMEDIATE);
                emit(static_cast<uint8_t>((value >> (i * 8)) & 0xFF));
                
                if (storage == StorageClass::Global) {
                    emitOp(Opcodes::STA_ABSOLUTE);
                    emitWord(static_cast<uint16_t>(offset + i));
                    addTextReloc(RelocationType::WORD, SegmentID::DATA);
                } else if (storage == StorageClass::ZeroPage) {
                    emitOp(Opcodes::STA_ZEROPAGE);
                    emit(static_cast<uint8_t>((offset + i) & 0xFF));
                }
            }
            break;
            
        default:
            throw std::runtime_error("Cannot store this type");
    }
}

const std::vector<uint8_t> CodegenVisitor::writeExportedGlobals()
{
    std::vector<uint8_t> global_bytes;

    for (auto x : scopes.front().symbols) {
        
    }
    

    return global_bytes;
}

void CodegenVisitor::emitPrologue(size_t frameSize)
{
    auto frame_pointer = getZeroPageAllocation("__frame_pointer").value();
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(frame_pointer.address);
    emitOp(Opcodes::PHA);

    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(frame_pointer.address + 1);
    emitOp(Opcodes::PHA);

    emitOp(Opcodes::TSX);

    emitOp(Opcodes::STX_ZEROPAGE);
    emit(frame_pointer.address);

    emitOp(Opcodes::LDA_IMMEDIATE);
    emit(0x01);

    emitOp(Opcodes::STA_ZEROPAGE);
    emit(frame_pointer.address + 1);
}

void CodegenVisitor::emitEpilogue(size_t frameSize)
{
    
    auto frame_pointer = getZeroPageAllocation("__frame_pointer").value();
    emitOp(Opcodes::LDX_ZEROPAGE);
    emit(frame_pointer.address);
    emitOp(Opcodes::TXS);

    emitOp(Opcodes::PLA);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(frame_pointer.address + 1);
    
    emitOp(Opcodes::PLA);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(frame_pointer.address);

}

void CodegenVisitor::loadIntoRegister(const EvaluationResult &source)
{
    bool is16Bit = getSizeOfType(source.type) == 2;

    switch (source.location) {
        case ValueLocation::Immediate: {
            emitOp(Opcodes::LDA_IMMEDIATE);
                emit((source.value) & 0xFF);
            if (is16Bit) {
                emitOp(Opcodes::LDX_IMMEDIATE);
                emit((source.value >> 8) & 0xFF);
            }
            break;
        }
        case ValueLocation::DataSegment: {
            emitOp(Opcodes::LDA_ABSOLUTE);
            emitWord(source.value);
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
            if (is16Bit) {
                // High Byte
                emitOp(Opcodes::LDX_ABSOLUTE);
                emitWord(source.value + 1); // Next byte
                addTextReloc(o65::RelocationType::WORD, o65::SegmentID::DATA);
            }
            break;
        }
        case ValueLocation::Accumulator:
            break;
    }
}

void CodegenVisitor::pushRegister(const AstType &type)
{
    const auto size = getSizeOfType(type);
    if (size == 1) {
        emitOp(Opcodes::PHA);
    } else if (size == 2) {
        emitOp(Opcodes::PHA);
        emitOp(Opcodes::PHX);
    } else {
        throw std::runtime_error("Push of types larger than 16-bit not yet supported");
    }
}
void CodegenVisitor::popRegister(const AstType &type)
{
    const auto size = getSizeOfType(type);
    if (size == 1) {
        emitOp(Opcodes::PLA);
    } else if (size == 2) {
        emitOp(Opcodes::PLA);
        emitOp(Opcodes::PLX);
    } else {
        throw std::runtime_error("Push of types larger than 16-bit not yet supported");
    }
}

void CodegenVisitor::emitConditionalBranch(uint8_t opcode, size_t labelId)
{
    uint8_t inverseOp;
    
    // 1. Map Opcode to Inverse
    switch (opcode) {
        case Opcodes::BEQ: inverseOp = Opcodes::BNE; break;
        case Opcodes::BNE: inverseOp = Opcodes::BEQ; break;
        case Opcodes::BCC: inverseOp = Opcodes::BCS; break; // Less than (unsigned)
        case Opcodes::BCS: inverseOp = Opcodes::BCC; break; // Greater/Eq (unsigned)
        default: throw std::runtime_error("Unsupported branch opcode");
    }
    emitOp(inverseOp);
    emit(0x3);
    emitJump(Opcodes::JMP_ABSOLUTE, labelId);
    // emitOp(Opcodes::JMP_ABSOLUTE);
    // emitWord(0x0000);
    
}

uint32_t CodegenVisitor::calculateMemberOffset(const AstType &structType, const std::string &memberName)
{
    auto structure = getStructDefinition(structType);
    size_t offset = 0;
    for (const auto& [name, member] : structure.members) {
        if (name == memberName) {
            return static_cast<uint32_t>(offset);
        }
        offset += getSizeOfType(member.second);
    }
    return 0;
}

void CodegenVisitor::visit(Node &node)
{
    throw std::runtime_error("CodegenVisitor: Encounted unknown or unhandled AST node type.");
}

void CodegenVisitor::visit(StringNode &node)
{
    const auto offset = dataSegment.size();
    addTextReloc(RelocationType::WORD, SegmentID::DATA);
    for (char c : node.value) {
        emitData(static_cast<uint8_t>(c));
    }
    emitData(0); // null term

    EvaluationResult result{};
    result.location = ValueLocation::DataSegment;
    result.value = static_cast<uint32_t>(offset);
    result.type = AstType::Primitive(TypeKind::STRING, 0); // implied pointer
    
    evalStack.push(result);
}

void CodegenVisitor::visit(NumberNode &node)
{
    int32_t value = node.value;

    TypeKind inferredKind = TypeKind::UNKNOWN;
    
    if (value >= 0) {
        // Unsigned values
        if (value <= 0xFF) {
            inferredKind = TypeKind::UNSIGNED_8;
        } else if (value <= 0xFFFF) {
            inferredKind = TypeKind::UNSIGNED_16;
        } else {
            inferredKind = TypeKind::UNSIGNED_32;
        }
    } else {
        // Signed values (negative)
        if (value >= -128 && value <= 127) {
            inferredKind = TypeKind::SIGNED_8;
        } else if (value >= -32768 && value <= 32767) {
            inferredKind = TypeKind::SIGNED_16;
        } else {
            inferredKind = TypeKind::SIGNED_32;
        }
    }

    EvaluationResult result{};
    result.location = ValueLocation::Immediate;
    result.value = static_cast<uint32_t>(value); // Reinterpret as unsigned for storage
    result.type = AstType::Primitive(inferredKind, 0); // 0 = not a pointer
    
    evalStack.push(result);
}

void CodegenVisitor::visit(UnaryNode &node)
{
    node.expression->accept(*this);

    auto result = evalStack.top();
    if (node.operation != UnaryOperator::REFERENCE)
        loadIntoRegister(result);
    evalStack.pop();

    switch (node.operation)
    {
    case UnaryOperator::NOT: {
        emitOp(Opcodes::CMP_IMMEDIATE);
        emit(0x00);
        break;
    }
    case UnaryOperator::NEGATE: {
        emitOp(Opcodes::EOR_IMMEDIATE);
        emit(0xFF);
        emitOp(Opcodes::CLC);
        emitOp(Opcodes::ADC_IMMEDIATE);
        emit(0x01);
        break;
    }
    case UnaryOperator::REFERENCE: {
        if (result.location == ValueLocation::DataSegment) {
            evalStack.push(EvaluationResult {ValueLocation::Immediate, result.value, AstType::PointerTo(result.type)});
        }
        break;
    }
    case UnaryOperator::DEREFERENCE: {
        emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT);
        emit(result.value);
        AstType dereferenced(result.type);
        dereferenced.decayPointer();
        evalStack.push(EvaluationResult {ValueLocation::Accumulator, 0, dereferenced});
        break;
    }
    
    default:
        break;
    }
}

void CodegenVisitor::visit(BoolNode& node)
{
    bool value = node.value;

    EvaluationResult result{};
    result.location = ValueLocation::Immediate;
    result.value = static_cast<uint32_t>(value);
    result.type = AstType::Primitive(TypeKind::BOOLEAN, 0); // 0 = not a pointer
    
    evalStack.push(result);
}

void CodegenVisitor::visit(AssignmentNode &node)
{
    node.expression->accept(*this);
    EvaluationResult exprResult = evalStack.top();
    evalStack.pop();

    if (dynamic_cast<ReferenceNode*>(node.assignee.get())) {
        auto refNode = dynamic_cast<ReferenceNode*>(node.assignee.get());
        std::string name = std::string(refNode->name);
        auto symOpt = findSymbol(name);
        if (!symOpt.has_value()) {
            throw std::runtime_error("Undefined variable in assignment: " + name);
        }
        auto& symbol = symOpt.value();
        loadIntoRegister(exprResult);
        saveRegisterToVariable(symbol);
    } else if (dynamic_cast<MemberReferenceNode*>(node.assignee.get())) {
        auto refNode = dynamic_cast<MemberReferenceNode*>(node.assignee.get());
        std::string base = std::string(refNode->base);
        std::string member = std::string(refNode->memberName);
        auto symOpt = findSymbol(base);
        if (!symOpt.has_value()) {
            throw std::runtime_error("Undefined variable in assignment: " + base);
        }

        auto& symbol = symOpt.value();
        loadIntoRegister(exprResult);
        saveRegisterToVariable(symbol, member);
    }
}

void CodegenVisitor::visit(BinaryOperationNode &node)
{
    node.left.get()->accept(*this);

    auto leftResult = evalStack.top();
    loadIntoRegister(leftResult);
    evalStack.pop();

    pushRegister(leftResult.type);

    node.right.get()->accept(*this);
    auto rightResult = evalStack.top(); evalStack.pop();


    bool is16BitArith = getSizeOfType(leftResult.type) > 1 || getSizeOfType(rightResult.type) > 1;
    bool isLeft16 = getSizeOfType(leftResult.type) > 1;
    bool isRight16 = getSizeOfType(rightResult.type) > 1;

    loadIntoRegister(rightResult);

    emitOp(Opcodes::STA_ZEROPAGE);
    emit(0);
    addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);

    
    if (is16BitArith && !isRight16) {
        emitOp(Opcodes::STX_ZEROPAGE);
        emit(0);
        addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
    }
    
    popRegister(leftResult.type);

    if (is16BitArith && !isLeft16) {
        emitOp(Opcodes::LDX_IMMEDIATE);
        emit(0);
    }


    switch (node.operation) {
        case Operator::ADD: {
            emitOp(Opcodes::CLC);
            emitOp(Opcodes::ADC_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emit(Opcodes::PHA);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::ADC_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::TAX);
            }
            emitOp(Opcodes::PLA);
            break;
        }
        case Operator::SUBTRACT: {
            emitOp(Opcodes::SEC);
            emitOp(Opcodes::SBC_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emit(Opcodes::PHA);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::SBC_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::TAX);
            }
            emitOp(Opcodes::PLA);
            break;
        }
        case Operator::MULTIPLY: { // TODO: create libraries for multiplication and division (not in 65c02 instructions)
            break;
        }
        case Operator::DIVIDE: {
            break;
        }
        case Operator::UNKNOWN:
            throw std::runtime_error("Unknown binary operator");
    }
    evalStack.push(EvaluationResult{ValueLocation::Accumulator, 0, leftResult.type});

}

void CodegenVisitor::visit(ComparisonNode &node)
{

    node.left.get()->accept(*this);

    auto leftResult = evalStack.top();
    loadIntoRegister(leftResult);
    evalStack.pop();

    pushRegister(leftResult.type);

    node.right.get()->accept(*this);
    auto rightResult = evalStack.top(); evalStack.pop();


    bool is16BitArith = getSizeOfType(leftResult.type) > 1 || getSizeOfType(rightResult.type) > 1;
    bool isLeft16 = getSizeOfType(leftResult.type) > 1;
    bool isRight16 = getSizeOfType(rightResult.type) > 1;

    loadIntoRegister(rightResult);

    emitOp(Opcodes::STA_ZEROPAGE);
    emit(0);
    addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);

    
    if (is16BitArith && !isRight16) {
        emitOp(Opcodes::STX_ZEROPAGE);
        emit(0);
        addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
    }
    
    popRegister(leftResult.type);

    if (is16BitArith && !isLeft16) {
        emitOp(Opcodes::LDX_IMMEDIATE);
        emit(0);
    }

    auto not_is_a_keyword = createLabel("not_equal");
    auto equal = createLabel("equal");
    switch (node.comparison) {
        case Comparison::EQUALS: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BNE, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BEQ, equal);
            }
            break;
        }
        case Comparison::NOT_EQUAL: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BEQ, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BNE, equal);
            }
            break;
        }
        case Comparison::LESS_THAN: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BNE, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BNE, not_is_a_keyword);
            }
            break;
        }
        case Comparison::GREATER_THAN: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BCC, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BCC, not_is_a_keyword);
            }
            break;
        }
        case Comparison::LESS_THAN_EQUAL: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BCC, equal);
            emitJump(Opcodes::BEQ, equal);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BCC, equal);
                emitJump(Opcodes::BEQ, equal);
            }
            emitJump(Opcodes::BRA, not_is_a_keyword);
            break;
        }
        case Comparison::GREATER_THAN_EQUAL: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(0);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BNE, equal);
            emitJump(Opcodes::BEQ, equal);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(0);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BNE, equal);
                emitJump(Opcodes::BEQ, equal);
            }
            emitJump(Opcodes::BRA, not_is_a_keyword);
            break;
        }
        case Comparison::UNKNOWN:
            throw std::runtime_error("Unknown binary operator");
    }
    emitLabel(equal);
    if (is16BitArith) {
        emitOp(Opcodes::TAX);
    }
    emitOp(Opcodes::LDA_IMMEDIATE);
    emit(0x1);
    emitLabel(not_is_a_keyword);
    if (is16BitArith) {
        emitOp(Opcodes::TAX);
    }
    emitOp(Opcodes::LDA_IMMEDIATE);
    emit(0x0);
    evalStack.push(EvaluationResult{ValueLocation::Accumulator, 0, leftResult.type});
}

void CodegenVisitor::visit(FunctionNode &node)
{
    auto name = std::string(node.name);
    if (node.defined) {
        SymbolInfo symbol{name, AstType::Primitive(TypeKind::UNKNOWN), StorageClass::Global, static_cast<int32_t>(textSegment.size()), true, 2}; // TODO: change with void, idk if size matters ngl

        exportedGlobals.push_back(std::make_pair(name, static_cast<int32_t>(textSegment.size())));
        
        enterScope();
        currentStackDepth = 0;
        currentFunction = symbol;
        auto frameSize = node.parameters.size(); // TODO: proper frame size calculation
        emitPrologue(frameSize); 

        for (auto &&n : node.body)
        {
            n->accept(*this);
        }
        

        emitEpilogue(frameSize);

        if (node.interrupt) {
            emitOp(Opcodes::RTI);
        } else {
            emitOp(Opcodes::RTS);
        }

        exitScope();
    } else {
        // getOrCreateUndefinedReference("_" + name);
        // NGL idk wtf to do here
    }
}

void CodegenVisitor::visit(StructureNode &node)
{
    size_t size = 0;
    std::map<std::string, std::pair<size_t, AstType>> members; 
    auto structName = std::string(node.name);
    for (const auto& [name, member] : node.members) {
        auto mSize = getSizeOfType(member);
        members.emplace(std::string(name), std::make_pair(mSize, member));
        size += mSize;
    }
    StructInfo structDef{structName, size, members};
    structDefinitions.emplace(std::string(node.name), structDef);
}

void CodegenVisitor::visit(StructureInitNode &node)
{
    throw std::runtime_error("Structure initialization not yet implemented"); // lmfao im not implementing this bullshit anytime soon 😂😂
}

void CodegenVisitor::visit(VariableNode &node)
{
    bool isLocal = scopes.size() > 1;
    declareVariable(std::string(node.name), node.type, (isLocal ? StorageClass::Stack : node.zeropaged ? StorageClass::ZeroPage : StorageClass::Global), getSizeOfType(node.type));
    
    if (!isLocal) {
        if (node.value.get() != nullptr) {
            node.value->accept(*this);
            auto result = evalStack.top(); evalStack.pop();
            if (result.location != ValueLocation::Immediate) {
                throw std::runtime_error("Non-constant value assigned to variable initialization");
            }
            emitData(result.value);
        } else {
            // Default initialize to zero
            size_t typeSize = getSizeOfType(node.type);
            for (size_t i = 0; i < typeSize; ++i) {
                emitData(0);
            }
        }
    } else {
        if (node.value.get() != nullptr) {
            node.value->accept(*this);
            auto result = evalStack.top(); evalStack.pop();
            storeNumberToMemory(result.value, currentStackDepth, StorageClass::Stack, result.type.kind);
        }
        currentStackDepth += getSizeOfType(node.type);
        
    }
}

void CodegenVisitor::visit(ReferenceNode &node)
{
    std::string name = std::string(node.name);
    auto sym = findSymbol(name);
    if (sym.has_value()) {
        auto& symbol = sym.value();
        EvaluationResult result{};
        result.location = ValueLocation::Accumulator;
        result.value = static_cast<uint32_t>(symbol.offset);
        result.type = symbol.type;

        size_t typeSize = getSizeOfType(symbol.type);

        switch (symbol.storage)
        {
        case StorageClass::Stack: {
            if (typeSize == 1) {
                auto temp = getZeroPageAllocation("__temporary").value().address;
                auto temp2 = getZeroPageAllocation("__temporary_2p").value().address;
                auto fp = getZeroPageAllocation("__frame_pointer").value().address;
                
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::SEC);
                
                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(symbol.offset & 0xFF);
                
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp2);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp+1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(0x00);

                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp2 + 1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);

            } else if (typeSize == 2) {
                auto temp = getZeroPageAllocation("__temporary").value().address;
                auto temp2 = getZeroPageAllocation("__temporary_2p").value().address;
                auto fp = getZeroPageAllocation("__frame_pointer").value().address;
                
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::SEC);
                
                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(symbol.offset & 0xFF);
                
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp2);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp+1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(0x00);

                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp2 + 1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);

                emitOp(Opcodes::PHA);

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x01);
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);
                emitOp(Opcodes::TAX);

                emitOp(Opcodes::PLA);
            } else {
                throw std::runtime_error("Multi-byte stack variables > 16 bits not yet supported");
            }
            break;
        }
        case StorageClass::Global: {
            if (typeSize == 1) {
                emitOp(Opcodes::LDA_ABSOLUTE);
                emitWord(static_cast<uint16_t>(symbol.offset));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
            } else if (typeSize == 2) {
                // Load low byte to A
                emitOp(Opcodes::LDA_ABSOLUTE);
                emitWord(static_cast<uint16_t>(symbol.offset));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
                
                // Load high byte to X
                emitOp(Opcodes::LDX_ABSOLUTE);
                emitWord(static_cast<uint16_t>(symbol.offset + 1));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
                
            } else {
                throw std::runtime_error("Multi-byte global variables > 16 bits not yet supported");
            }
            break;
        }
        case StorageClass::ZeroPage: {
            if (typeSize == 1) {
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(static_cast<uint8_t>(symbol.offset & 0xFF));
                // Add relocation: references a byte in the ZERO segment
                addTextReloc(RelocationType::LOW, SegmentID::ZERO); 
            } else if (typeSize == 2) {
                // Load low byte
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(static_cast<uint8_t>(symbol.offset & 0xFF));
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::PHA);
                
                // Load high byte
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(static_cast<uint8_t>((symbol.offset + 1) & 0xFF));
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::TAX);
                
                emitOp(Opcodes::PLA);
            } else {
                throw std::runtime_error("Multi-byte zero page variables > 16 bits not yet supported");
            }
            break;
        }
        default:
            break;
        }
        
        evalStack.push(result);
    } else {
        throw std::runtime_error("Unknown variable");
    }
}

void CodegenVisitor::visit(CallNode &node)
{
    auto funcSymOpt = findSymbol(std::string(node.name));

    if (!funcSymOpt.has_value()) {
        throw std::runtime_error("Undefined function: " + std::string(node.name));
    }

    for (auto&& param : node.parameters) {
        param->accept(*this);
        loadIntoRegister(evalStack.top());
        pushRegister(evalStack.top().type);
        evalStack.pop();
    }

    emitOp(Opcodes::JSR);
    emitWord(static_cast<uint16_t>(funcSymOpt->offset));

    pushRegister(AstType::Primitive(TypeKind::UNSIGNED_16)); // Return address pushed onto stack
}

void CodegenVisitor::visit(IfNode &node)
{
    auto elseLabel = createLabel("else");
    auto endLabel = createLabel("endif");

    node.condition->accept(*this);
    auto conditionResult = evalStack.top();
    loadIntoRegister(conditionResult);
    evalStack.pop();

    emitOp(Opcodes::CMP_IMMEDIATE);
    emit(0x00);
    emitJump(Opcodes::BEQ, elseLabel);

    for (auto&& stmt : node.body) {
        stmt->accept(*this);
    }

    emitJump(Opcodes::JMP_ABSOLUTE, endLabel);
    emitLabel(elseLabel);
    for (auto&& stmt : node.elseBody) {
        stmt->accept(*this);
    }
    emitLabel(endLabel);
}

void CodegenVisitor::visit(WhileNode &node)
{
    auto startLabel = createLabel("start");
    auto endLabel = createLabel("end");

    emitLabel(startLabel);
    node.condition->accept(*this);
    auto conditionResult = evalStack.top();
    loadIntoRegister(conditionResult);
    evalStack.pop();
    emitOp(Opcodes::CMP_IMMEDIATE);
    emit(0x00);
    emitJump(Opcodes::BEQ, endLabel);

    for (auto&& stmt : node.body) {
        stmt->accept(*this);
    }

    emitJump(Opcodes::JMP_ABSOLUTE, startLabel);

    emitLabel(endLabel);
}

void CodegenVisitor::visit(MemberReferenceNode &node)
{
    std::string baseName = std::string(node.base);
    std::string memberName = std::string(node.memberName);
    auto symOpt = findSymbol(baseName);
    if (!symOpt.has_value()) {
        throw std::runtime_error("Undefined variable in member reference: " + baseName);
    }
    auto& symbol = symOpt.value();

    size_t memberOffset = calculateMemberOffset(symbol.type, memberName);

    EvaluationResult result{};
    result.location = ValueLocation::Accumulator;
    result.value = static_cast<uint32_t>(symbol.offset + memberOffset);
    auto structType = symbol.type;
    structType.decayPointer();
    auto structDef = getStructDefinition(structType);
    result.type = structDef.members[memberName].second;

    size_t typeSize = getSizeOfType(result.type);

    switch (symbol.storage)
    {
    case StorageClass::Global: {
        if (typeSize == 1) {
            emitOp(Opcodes::LDA_ABSOLUTE);
            emitWord(static_cast<uint16_t>(symbol.offset + memberOffset));
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
        } else if (typeSize == 2) {
            // Load low byte to A
            emitOp(Opcodes::LDA_ABSOLUTE);
            emitWord(static_cast<uint16_t>(symbol.offset + memberOffset));
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
            
            // Load high byte to X
            emitOp(Opcodes::LDX_ABSOLUTE);
            emitWord(static_cast<uint16_t>(symbol.offset + memberOffset + 1));
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
            
        } else {
            throw std::runtime_error("Multi-byte global variables > 16 bits not yet supported");
        }
        break;
    }
    default:
        throw std::runtime_error("Member references for non-global variables not yet supported");
    }
    
    evalStack.push(result);
}

std::vector<uint8_t> CodegenVisitor::generateO65()
{
    std::vector<uint8_t> o65Data;
    implementation_defined::O65_File_Layout layout;

    layout.mode = getMode();
    layout.text_data = textSegment.data();
    layout.data_data = dataSegment.data();
    layout.next_section = nullptr;

    return o65Data;
}

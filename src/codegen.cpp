#include "codegen.hpp"
#include <stdexcept>
#include <cstring>
// #include <iostream>
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
            
        case StorageClass::Stack: {
            // Load from stack frame using frame pointer indirect addressing
            // Stack variables have negative offsets from frame pointer
            // We use LDA (zp),Y where zp = frame pointer and Y = offset
            auto fp = getZeroPageAllocation("__frame_pointer").value().address;
            
            // For positive offsets (parameters), we can use (FP),Y directly
            // For negative offsets (locals), we need to compute FP - offset into temp
            if (sym.offset >= 0) {
                // Parameter - positive offset from FP
                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(static_cast<uint8_t>(sym.offset));
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(fp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            } else {
                // Local variable - negative offset, need to compute address
                auto temp = getZeroPageAllocation("__temporary_2p").value().address;
                
                // temp = FP - |offset|
                emitOp(Opcodes::SEC);
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(static_cast<uint8_t>((-sym.offset) & 0xFF));
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp + 1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp + 1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                // Now load through temp
                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            }
            break;
        }
            
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
            
        case StorageClass::Stack: {
            // Store to stack frame using frame pointer indirect addressing
            auto fp = getZeroPageAllocation("__frame_pointer").value().address;
            
            if (sym.offset >= 0) {
                // Parameter - positive offset from FP
                // Save A first, then use it for addressing
                emitOp(Opcodes::PHA);
                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(static_cast<uint8_t>(sym.offset));
                emitOp(Opcodes::PLA);
                emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
                emit(fp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            } else {
                // Local variable - negative offset
                auto temp = getZeroPageAllocation("__temporary_2p").value().address;
                auto tempA = getZeroPageAllocation("__temporary").value().address;
                
                // Save A to temp
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(tempA);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                // Compute FP - |offset| into temp2
                emitOp(Opcodes::SEC);
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(static_cast<uint8_t>((-sym.offset) & 0xFF));
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(fp + 1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::SBC_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(temp + 1);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                
                // Load A back and store through temp
                emitOp(Opcodes::LDA_ZEROPAGE);
                emit(tempA);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            }
            break;
        }
            
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

            
            if (storage == StorageClass::Global) {
                emitOp(Opcodes::LDA_IMMEDIATE);
                emit(static_cast<uint8_t>(value & 0xFF));
                emitOp(Opcodes::STA_ABSOLUTE); // 0x8D
                emitWord(static_cast<uint16_t>(offset));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
            } else if (storage == StorageClass::ZeroPage) {
                emitOp(Opcodes::LDA_IMMEDIATE);
                emit(static_cast<uint8_t>(value & 0xFF));
                emitOp(Opcodes::STA_ZEROPAGE); // 0x85
                emit(static_cast<uint8_t>(offset & 0xFF));
            } else if (storage == StorageClass::Stack) {
                // auto temp = getZeroPageAllocation("__temporary").value().address;
                auto temp2 = getZeroPageAllocation("__temporary_2p").value().address;
                auto fp = getZeroPageAllocation("__frame_pointer").value().address;

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

                emitOp(Opcodes::LDA_IMMEDIATE);
                emit(static_cast<uint8_t>(value & 0xFF));

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);

                emitOp(Opcodes::PHA);

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x01);
                emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);
                emitOp(Opcodes::TAX);

                emitOp(Opcodes::PLA);


                

            }
            break;
            
        case TypeKind::UNSIGNED_16:
        case TypeKind::SIGNED_16:
            // 16-bit store - low byte then high byte
            
            // Store low byte
            
            if (storage == StorageClass::Global) {
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit(static_cast<uint8_t>(value & 0xFF));
                emitOp(Opcodes::STA_ABSOLUTE);
                emitWord(static_cast<uint16_t>(offset));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
            } else if (storage == StorageClass::ZeroPage) {
                
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit(static_cast<uint8_t>(value & 0xFF));
                emitOp(Opcodes::STA_ZEROPAGE);
                emit(static_cast<uint8_t>(offset & 0xFF));
            } else if (storage == StorageClass::Stack) {
                                auto temp2 = getZeroPageAllocation("__temporary_2p").value().address;
                auto fp = getZeroPageAllocation("__frame_pointer").value().address;

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

                emitOp(Opcodes::LDA_IMMEDIATE);
                emit(static_cast<uint8_t>(value & 0xFF));

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x00);
                emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);


                emitOp(Opcodes::LDA_IMMEDIATE);
                emit(static_cast<uint8_t>((value >> 8) & 0xFF));

                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x01);
                emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
                emit(temp2);

            }
            
            // Store high byte
            
            if (storage == StorageClass::Global) {
                
                emitOp(Opcodes::LDA_IMMEDIATE);
                emit(static_cast<uint8_t>((value >> 8) & 0xFF));
                emitOp(Opcodes::STA_ABSOLUTE);
                emitWord(static_cast<uint16_t>(offset + 1));
                addTextReloc(RelocationType::WORD, SegmentID::DATA);
            } else if (storage == StorageClass::ZeroPage) {
                
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit(static_cast<uint8_t>((value >> 8) & 0xFF));
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

const std::vector<uint8_t> CodegenVisitor::writeUndefinedReferences()
{
    std::vector<uint8_t> undef_bytes;
    size_t total_size = undefinedList.size();
    undef_bytes.push_back(static_cast<uint8_t>(total_size & 0xFF));
    undef_bytes.push_back(static_cast<uint8_t>((total_size >> 8) & 0xFF)); // assuming little endian
    for (const auto& name : undefinedList) {
        // Write null-terminated string
        for (char c : name) {
            undef_bytes.push_back(static_cast<uint8_t>(c));
        }
        undef_bytes.push_back(0x00); // Null terminator
    }

    return undef_bytes;
}

const std::vector<uint8_t> CodegenVisitor::writeRelocationTable(const std::vector<o65::O65_Relocation_Entry> relocations, const uint32_t base)
{
    std::vector<uint8_t> reloc_bytes;

    uint32_t last = base - 1;

    for (auto &&reloc : relocations)
    {
        uint32_t offset = reloc.absolute_offset - last;
        while (offset > RELOCATION_OFFSET_MAX) {
            reloc_bytes.push_back(RELOCATION_OFFSET_CONTINUE);
            offset -= RELOCATION_OFFSET_INCREMENT;
        }
        reloc_bytes.push_back(static_cast<uint8_t>(offset & 0xFF));

        reloc_bytes.push_back(reloc.typebyte);

        // Symbol index is required for UNDEFINED segment references (segment ID 0)
        // The segment ID is in the lower 5 bits of the typebyte
        if ((get_segment_id(reloc.typebyte)) == o65::SegmentID::UNDEFINED) {
            reloc_bytes.push_back(reloc.symbol_index & 0xFF);
            reloc_bytes.push_back((reloc.symbol_index >> 8) & 0xFF);
        }

        if (reloc.low_byte) {
            reloc_bytes.push_back(reloc.low_byte);
        }
        if (reloc.seg_offset) {
            reloc_bytes.push_back(reloc.seg_offset & 0xFF);
            reloc_bytes.push_back((reloc.seg_offset >> 8) & 0xFF); // little endian i think
        }
        last = reloc.absolute_offset;
    }

    reloc_bytes.push_back(RELOCATION_TABLE_TERMINATOR); 
    

    return reloc_bytes;
}

const std::vector<uint8_t> CodegenVisitor::writeExportedGlobals()
{
    std::vector<uint8_t> global_bytes;
    
    // Merge symbols from global scope into exportedGlobals
    auto& globals = scopes.front().symbols;
    for (const auto& [name, sym] : globals) {
        // Determine if this is a function or variable
        // Functions are marked as isConstant and have storage Global with offset in TEXT
        // Variables have storage Global/ZeroPage with offset in DATA/ZERO
        
        // Check if already in exportedGlobals to avoid duplicates
        bool alreadyExported = false;
        for (const auto& [expName, expOffset] : exportedGlobals) {
            if (expName == name) {
                alreadyExported = true;
                break;
            }
        }
        
        if (!alreadyExported && !sym.isExternal) {
            exportedGlobals.push_back({name, sym.offset});
        }
    }
    
    size_t total_size = exportedGlobals.size();

    global_bytes.push_back(static_cast<uint8_t>(total_size & 0xFF));
    global_bytes.push_back(static_cast<uint8_t>((total_size >> 8) & 0xFF)); // assuming little endian
    
    for (const auto& [name, offset] : exportedGlobals) {
        // Write null-terminated name
        for (char c : name) {
            global_bytes.push_back(static_cast<uint8_t>(c));
        }
        global_bytes.push_back(0x00u);

        // Determine segment ID based on symbol properties
        auto symOpt = findSymbol(name);
        if (symOpt.has_value()) {
            const auto& sym = symOpt.value();
            
            // Functions are isConstant=true, variables are isConstant=false
            if (sym.isConstant) {
                // Function - lives in TEXT segment
                global_bytes.push_back(static_cast<uint8_t>(SegmentID::TEXT));
            } else if (sym.storage == StorageClass::ZeroPage) {
                // Zero page variable
                global_bytes.push_back(static_cast<uint8_t>(SegmentID::ZERO));
            } else {
                // Global variable - lives in DATA segment
                global_bytes.push_back(static_cast<uint8_t>(SegmentID::DATA));
            }
        } else {
            // Default to TEXT for unknown (likely interrupt handlers added directly)
            global_bytes.push_back(static_cast<uint8_t>(SegmentID::TEXT));
        }

        global_bytes.push_back(static_cast<uint8_t>(offset & 0xFF));
        global_bytes.push_back(static_cast<uint8_t>((offset >> 8) & 0xFF)); // little endian
    }

    return global_bytes;
}

void CodegenVisitor::emitPrologue(size_t)
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

void CodegenVisitor::emitEpilogue(size_t)
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
        case ValueLocation::DereferencedPointer: {
            // source.value contains the zero page address where the pointer is stored
            uint8_t zpAddr = static_cast<uint8_t>(source.value & 0xFF);
            
            // Load through indirect addressing: LDA (zp),Y with Y=0
            emitOp(Opcodes::LDY_IMMEDIATE);
            emit(0x00);
            emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
            emit(zpAddr);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            if (is16Bit) {
                // Save low byte, load high byte
                emitOp(Opcodes::PHA);
                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x01);
                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
                emit(zpAddr);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::TAX);
                emitOp(Opcodes::PLA);
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

void CodegenVisitor::visit(Node &)
{
    throw std::runtime_error("CodegenVisitor: Encountered unknown or unhandled AST node type.");
}

void CodegenVisitor::visit(IncludeNode &)
{
    return; // should be handled in earlier pass
}

void CodegenVisitor::visit(DefineNode &)
{
    return; // should be handled in earlier pass
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
    // Handle REFERENCE operator specially - we need the address, not the value
    if (node.operation == UnaryOperator::REFERENCE) {
        if (auto* refNode = dynamic_cast<ReferenceNode*>(node.expression.get())) {
            std::string name = std::string(refNode->name);
            auto sym = findSymbol(name);
            if (!sym.has_value()) {
                throw std::runtime_error("Undefined variable: " + name);
            }
            auto& symbol = sym.value();
            
            // Create a pointer type from the variable's type
            AstType ptrType = AstType::PointerTo(symbol.type);
            
            switch (symbol.storage) {
                case StorageClass::Global:
                    // Address is the data segment offset
                    evalStack.push(EvaluationResult {
                        ValueLocation::DataSegment,  
                        static_cast<uint32_t>(symbol.offset), 
                        ptrType
                    });
                    break;
                    
                case StorageClass::ZeroPage:
                    // Address is the zero page offset - fits in 16-bit pointer
                    emitOp(Opcodes::LDA_IMMEDIATE);
                    emit(static_cast<uint8_t>(symbol.offset & 0xFF));
                    emitOp(Opcodes::LDX_IMMEDIATE);
                    emit(0x00); // High byte is 0 for zero page
                    evalStack.push(EvaluationResult {
                        ValueLocation::Accumulator,
                        static_cast<uint32_t>(symbol.offset),
                        ptrType
                    });
                    break;
                    
                case StorageClass::Stack: {
                    // Address is computed from frame pointer
                    auto temp = getZeroPageAllocation("__temporary_2p").value().address;
                    auto fp = getZeroPageAllocation("__frame_pointer").value().address;
                    
                    // Compute FP - offset
                    emitOp(Opcodes::LDA_ZEROPAGE);
                    emit(fp);
                    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                    emitOp(Opcodes::SEC);
                    emitOp(Opcodes::SBC_IMMEDIATE);
                    emit(static_cast<uint8_t>((-symbol.offset) & 0xFF));
                    emitOp(Opcodes::STA_ZEROPAGE);
                    emit(temp);
                    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                    
                    emitOp(Opcodes::LDA_ZEROPAGE);
                    emit(fp + 1);
                    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                    emitOp(Opcodes::SBC_IMMEDIATE);
                    emit(0x00);
                    emitOp(Opcodes::TAX);
                    
                    emitOp(Opcodes::LDA_ZEROPAGE);
                    emit(temp);
                    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
                    
                    evalStack.push(EvaluationResult {
                        ValueLocation::Accumulator,
                        0, // Address is in A/X
                        ptrType
                    });
                    break;
                }
                    
                default:
                    throw std::runtime_error("Cannot take reference of this storage class");
            }
            return;
        }
        // For other expressions, fall through to normal handling
    }
    
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
    // case UnaryOperator::REFERENCE: {
    //     // Taking the address of a value - result is a pointer
    //     // The result.value already contains the offset/address from the ReferenceNode visitor
    //     if (result.location == ValueLocation::DataSegment || 
    //         result.location == ValueLocation::Immediate) {
    //         // For global data segment variables, result.value is the offset in data segment
    //         evalStack.push(EvaluationResult {ValueLocation::Immediate, result.value, AstType::PointerTo(result.type)});
    //     } else if (result.location == ValueLocation::Accumulator) {
    //         // The address was loaded into A (and possibly X for 16-bit)
    //         // This typically happens for zero page or other already-computed addresses
    //         evalStack.push(EvaluationResult {ValueLocation::Accumulator, result.value, AstType::PointerTo(result.type)});
    //     } else {
    //         throw std::runtime_error("Cannot take reference of this value location");
    //     }
    //     break;
    // }
        case UnaryOperator::DEREFERENCE: {
        // For dereference, we need to preserve the pointer value itself
        // so it can be used for both reading and writing
        // The result type needs to have its pointer level decremented
        AstType dereferenced = result.type.decayPointer();
        
        auto temp = getZeroPageAllocation("__temporary_2p").value().address;
        
        if (result.location == ValueLocation::Immediate) {
            // Pointer is a constant address - load it into zero page temp
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit(result.value & 0xFF);
            emitOp(Opcodes::STA_ZEROPAGE);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            emitOp(Opcodes::LDA_IMMEDIATE);
            emit((result.value >> 8) & 0xFF);
            emitOp(Opcodes::STA_ZEROPAGE);
            emit(temp + 1);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            evalStack.push(EvaluationResult {ValueLocation::DereferencedPointer, 
                            static_cast<uint32_t>(temp), 
                            dereferenced});
        } else if (result.location == ValueLocation::DataSegment) {
            // Pointer is stored in data segment - load pointer value into temp
            emitOp(Opcodes::LDA_ABSOLUTE);
            emitWord(static_cast<uint16_t>(result.value));
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
            emitOp(Opcodes::STA_ZEROPAGE);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            emitOp(Opcodes::LDA_ABSOLUTE);
            emitWord(static_cast<uint16_t>(result.value + 1));
            addTextReloc(RelocationType::WORD, SegmentID::DATA);
            emitOp(Opcodes::STA_ZEROPAGE);
            emit(temp + 1);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            evalStack.push(EvaluationResult {ValueLocation::DereferencedPointer, 
                            static_cast<uint32_t>(temp), 
                            dereferenced});
        } else if (result.location == ValueLocation::Accumulator) {
            // Pointer value is already in A (low) and X (high)
            emitOp(Opcodes::STA_ZEROPAGE);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            emitOp(Opcodes::STX_ZEROPAGE);
            emit(temp + 1);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            evalStack.push(EvaluationResult {ValueLocation::DereferencedPointer, 
                            static_cast<uint32_t>(temp), 
                            dereferenced});
        } else {
            throw std::runtime_error("Cannot dereference this value location");
        }
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
    // Check if this is a dereference assignment (@ptr = value)
    if (auto* unaryNode = dynamic_cast<UnaryNode*>(node.assignee.get())) {
        if (unaryNode->operation == UnaryOperator::DEREFERENCE) {
            // First evaluate the pointer expression
            unaryNode->expression->accept(*this);
            EvaluationResult ptrResult = evalStack.top();
            evalStack.pop();
            
            // Store pointer in zero page temp
            auto temp = getZeroPageAllocation("__temporary_2p").value().address;
            loadIntoRegister(ptrResult);
            emitOp(Opcodes::STA_ZEROPAGE);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            emitOp(Opcodes::STX_ZEROPAGE);
            emit(temp + 1);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            // Now evaluate the expression to assign
            node.expression->accept(*this);
            EvaluationResult exprResult = evalStack.top();
            evalStack.pop();
            
            // Load expression value into accumulator
            loadIntoRegister(exprResult);
            
            // Store through pointer indirection: STA (zp),Y with Y=0
            emitOp(Opcodes::LDY_IMMEDIATE);
            emit(0x00);
            emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            
            // Handle 16-bit stores: STA (zp),Y with Y=1
            if (getSizeOfType(exprResult.type) == 2) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::LDY_IMMEDIATE);
                emit(0x01);
                emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
                emit(temp);
                addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            }
            return;
        }
    }

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

    auto temp = getZeroPageAllocation("__temporary").value().address;
    auto temp2 = getZeroPageAllocation("__temporary_dos").value().address;
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(temp);
    addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);

    
    if (is16BitArith && !isRight16) {
        emitOp(Opcodes::PHX);
        emitOp(Opcodes::LDX_IMMEDIATE);
        emit(0);
        emitOp(Opcodes::STX_ZEROPAGE);
        emit(temp2);
        addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::PLX);
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
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emit(Opcodes::PHA);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::ADC_ZEROPAGE);
                emit(temp2);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::TAX);
            }
            emitOp(Opcodes::PLA);
            break;
        }
        case Operator::SUBTRACT: {
            emitOp(Opcodes::SEC);
            emitOp(Opcodes::SBC_ZEROPAGE);
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emit(Opcodes::PHA);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::SBC_ZEROPAGE);
                emit(temp2);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitOp(Opcodes::TAX);
            }
            emitOp(Opcodes::PLA);
            break;
        }
        case Operator::MULTIPLY: {
            // 8-bit multiplication: A * temp -> A:X (low:high)
            // Left operand is in A, right operand is in temp
            // Use shift-and-add or table-based multiply
            #ifdef USE_TABLE_MULTIPLY
            emitMultiply8Table();
            #else
            emitMultiply8Shift();
            #endif
            break;
        }
        case Operator::DIVIDE: {
            // 8-bit division: A / temp -> A (quotient), X (remainder)
            emitDivide8();
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

    auto temp = getZeroPageAllocation("__temporary").value().address;
    auto temp2 = getZeroPageAllocation("__temporary_dos").value().address;
    
    loadIntoRegister(rightResult);

    emitOp(Opcodes::STA_ZEROPAGE);
    emit(temp);
    addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);

    
    if (is16BitArith && !isRight16) {
        emitOp(Opcodes::PHX);
        emitOp(Opcodes::LDX_IMMEDIATE);
        emit(0);
        emitOp(Opcodes::STX_ZEROPAGE);
        emit(temp2);
        addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::PLX);
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
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BNE, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(temp2);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BEQ, equal);
            }
            break;
        }
        case Comparison::NOT_EQUAL: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BEQ, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(temp2);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BNE, equal);
            }
            break;
        }
        case Comparison::LESS_THAN: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BNE, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(temp2);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BNE, not_is_a_keyword);
            }
            break;
        }
        case Comparison::GREATER_THAN: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BCC, not_is_a_keyword);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(temp2);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BCC, not_is_a_keyword);
            }
            break;
        }
        case Comparison::LESS_THAN_EQUAL: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BCC, equal);
            emitJump(Opcodes::BEQ, equal);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(temp2);
                addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
                emitJump(Opcodes::BCC, equal);
                emitJump(Opcodes::BEQ, equal);
            }
            emitJump(Opcodes::BRA, not_is_a_keyword);
            break;
        }
        case Comparison::GREATER_THAN_EQUAL: {
            emitOp(Opcodes::CMP_ZEROPAGE);
            emit(temp);
            addTextReloc(o65::RelocationType::LOW, SegmentID::ZERO);
            emitJump(Opcodes::BNE, equal);
            emitJump(Opcodes::BEQ, equal);
            if (is16BitArith) {
                emitOp(Opcodes::TXA);
                emitOp(Opcodes::CMP_ZEROPAGE);
                emit(temp2);
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
        SymbolInfo symbol{name, AstType::Primitive(TypeKind::UNKNOWN), StorageClass::Global, static_cast<int32_t>(textSegment.size()), true, false, 2};

        exportedGlobals.push_back(std::make_pair(name, static_cast<int32_t>(textSegment.size())));
        
        // Track interrupt handler assignments for vector table
        switch (node.interruptType) {
            case InterruptType::NMI:
                nmiHandler = name;
                break;
            case InterruptType::RESET:
                resetHandler = name;
                break;
            case InterruptType::IRQ:
                irqHandler = name;
                break;
            case InterruptType::NONE:
                break;
        }
        
        enterScope();
        currentStackDepth = 0;
        currentFunction = symbol;
        currentFunctionIsInterrupt = node.isInterrupt();
        
        // Calculate frame size from parameters
        size_t frameSize = 0;
        for (const auto& [paramName, paramType] : node.parameters) {
            frameSize += getSizeOfType(paramType);
        }
        
        if (node.isInterrupt()) { 
            // Save registers for interrupt handler
            emitOp(Opcodes::PHA);
            emitOp(Opcodes::PHX);
            emitOp(Opcodes::PHY);
        } 
        else {
            emitPrologue(frameSize);
            
            // Declare parameters as stack variables
            // Parameters are pushed right-to-left, so first param is closest to FP
            int32_t paramOffset = 4; // Skip saved FP (2) + return addr (2)
            for (const auto& [paramName, paramType] : node.parameters) {
                size_t paramSize = getSizeOfType(paramType);
                SymbolInfo paramSym{std::string(paramName), paramType, StorageClass::Stack, paramOffset, false, false, paramSize};
                scopes.back().symbols[std::string(paramName)] = paramSym;
                paramOffset += paramSize;
            }
        }

        for (auto &&n : node.body)
        {
            n->accept(*this);
        }
        
        // Only emit epilogue if not already returned
        if (!node.isInterrupt()) {
            emitEpilogue(frameSize);
            emitOp(Opcodes::RTS);
        } else {
            // Restore registers for interrupt handler
            emitOp(Opcodes::PLY);
            emitOp(Opcodes::PLX);
            emitOp(Opcodes::PLA);
            emitOp(Opcodes::RTI);
        }

        exitScope();
        currentFunctionIsInterrupt = false;
    } else {
        size_t undefIndex = getOrCreateUndefinedReference(name);
        
        // Register symbol so it can be called
        SymbolInfo symbol{name, AstType::Primitive(TypeKind::UNKNOWN), 
                         StorageClass::Global, 
                         static_cast<int32_t>(undefIndex),
                         true, true, 2}; // isExternal = true
        scopes.back().symbols[name] = symbol;
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
    // Structure initialization creates a compound value in the data segment
    // Each member is initialized in order
    
    // For global initialization, emit member values to data segment
    // For local initialization, this would push values onto stack
    
    size_t startOffset = dataSegment.size();
    
    // We need the struct type info - but StructureInitNode doesn't have type info
    // This needs to be inferred from context (variable declaration)
    // For now, emit members in declaration order with their values
    
    for (const auto& [memberName, memberExpr] : node.members) {
        if (memberExpr) {
            memberExpr->accept(*this);
            auto result = evalStack.top();
            evalStack.pop();
            
            if (result.location == ValueLocation::Immediate) {
                // Emit the value to data segment
                size_t size = getSizeOfType(result.type);
                for (size_t i = 0; i < size; ++i) {
                    emitData(static_cast<uint8_t>((result.value >> (i * 8)) & 0xFF));
                }
            } else {
                throw std::runtime_error("Non-constant value in structure initializer for member: " + std::string(memberName));
            }
        }
    }
    
    // Push result as data segment location
    EvaluationResult result{};
    result.location = ValueLocation::DataSegment;
    result.value = static_cast<uint32_t>(startOffset);
    result.type = AstType::Primitive(TypeKind::STRUCTURE);
    evalStack.push(result);
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

                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT);
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

                emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT);
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
        throw std::runtime_error("Undeclared function: " + std::string(node.name));
    }

    for (auto&& param : node.parameters) {
        param->accept(*this);
        loadIntoRegister(evalStack.top());
        pushRegister(evalStack.top().type);
        evalStack.pop();
    }

    emitOp(Opcodes::JSR);
    emitWord(static_cast<uint16_t>(funcSymOpt->offset));
    if (funcSymOpt->isExternal) {
        // Emit JSR with relocation to undefined symbol
        emitOp(Opcodes::JSR);
        emitWord(0x0000); // Placeholder
        // Add relocation entry pointing to undefined symbol
        addTextReloc(RelocationType::WORD, SegmentID::UNDEFINED);
        // The relocation entry needs the symbol index
        textRelocs.back().symbol_index = funcSymOpt->offset; // undefined index
    } else {
        emitOp(Opcodes::JSR);
        emitWord(static_cast<uint16_t>(funcSymOpt->offset));
    }

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

const std::vector<uint8_t> serializeO65(const implementation_defined::O65_File_Layout &layout)
{
    std::vector<uint8_t> o65Data;

    // Serialize Header
    const O65_Header_16* header = reinterpret_cast<const O65_Header_16*>(layout.header);
    const uint8_t* headerPtr = reinterpret_cast<const uint8_t*>(layout.header);
    o65Data.insert(o65Data.end(), headerPtr, headerPtr + sizeof(O65_Header_16));

    
    // Serialize Header Options
    // First, serialize the options terminator (0x00)
    o65Data.push_back(o65::HEADER_OPTIONS_TERMINATOR);

    // Serialize Text Segment
    if (layout.text_data != nullptr && header->tlen > 0) {
        o65Data.insert(o65Data.end(), layout.text_data, layout.text_data + header->tlen);
    } 

    // Serialize Data Segment
    if (layout.data_data != nullptr && header->dlen > 0) {
        o65Data.insert(o65Data.end(), layout.data_data, layout.data_data + header->dlen);
    }

    // Serialize Undefined References
    if (layout.undef_refs.data != nullptr && layout.undef_refs.size_bytes > 0) {
        o65Data.insert(o65Data.end(), layout.undef_refs.data, layout.undef_refs.data + layout.undef_refs.size_bytes);
    } else {
        o65Data.push_back(0x00); // Count = 0 (low byte)
        o65Data.push_back(0x00); // Count = 0 (high byte)
    }

    // Serialize Relocation Tables
    if (layout.text_reloc.data != nullptr && layout.text_reloc.size_bytes > 0) {
        o65Data.insert(o65Data.end(), layout.text_reloc.data, layout.text_reloc.data + layout.text_reloc.size_bytes);
    } else {
        o65Data.push_back(o65::RELOCATION_TABLE_TERMINATOR); // Terminator only
    }
    if (layout.data_reloc.data != nullptr && layout.data_reloc.size_bytes > 0) {
        o65Data.insert(o65Data.end(), layout.data_reloc.data, layout.data_reloc.data + layout.data_reloc.size_bytes);
    } else {
        o65Data.push_back(o65::RELOCATION_TABLE_TERMINATOR); // Terminator only
    }

    // Serialize Exported Globals
    if (layout.exported_globals.data != nullptr && layout.exported_globals.size_bytes > 0) {
        o65Data.insert(o65Data.end(), layout.exported_globals.data, layout.exported_globals.data + layout.exported_globals.size_bytes);
    } else {
        o65Data.push_back(0x00); // Count = 0 (low byte)
        o65Data.push_back(0x00); // Count = 0 (high byte)
    }

    if (layout.next_section != nullptr) {
        auto nextSectionData = serializeO65(*layout.next_section);
        o65Data.insert(o65Data.end(), nextSectionData.begin(), nextSectionData.end());
    }

    return o65Data;
}

const std::vector<uint8_t> CodegenVisitor::buildHeaderOptions()
{
    std::vector<uint8_t> options;
    
    // Option: Assembler/linker identification (type 2)
    const char* assemblerName = "nixie";
    size_t nameLen = strlen(assemblerName);
    uint8_t optionLen = static_cast<uint8_t>(nameLen + 2); // +2 for length byte and type byte
    options.push_back(optionLen);
    options.push_back(static_cast<uint8_t>(o65::HeaderOptionType::ASSEMBLER));
    for (size_t i = 0; i < nameLen; ++i) {
        options.push_back(static_cast<uint8_t>(assemblerName[i]));
    }
    
    // Add more options here as needed (filename, author, etc.)
    
    // Terminate options list
    options.push_back(o65::HEADER_OPTIONS_TERMINATOR);
    
    return options;
}

std::vector<uint8_t> CodegenVisitor::generateO65()
{
    // std::vector<uint8_t> o65Data;
    implementation_defined::O65_File_Layout layout;
    O65_Header_16 header16{};

    layout.mode = getMode();

    std::memcpy(header16.marker, o65::NON_C64_MARKER, 2);
    std::memcpy(header16.magic, o65::MAGIC_NUMBER, 3);

    header16.version = 0x00;
    header16.mode = layout.mode.encode();
    header16.tbase = 0x0000; // Text segment load address (set by loader)
    header16.dbase = 0x0000; // Data segment load address (set by loader)
    header16.zbase = 0x0000; // Zero page segment load address (set by loader)
    header16.bbase = 0x0000; // BSS segment load address (set by loader)
    header16.tlen = static_cast<uint16_t>(textSegment.size());
    header16.dlen = static_cast<uint16_t>(dataSegment.size());
    header16.zlen = static_cast<uint16_t>(zeroPageAllocator.totalAllocated());
    header16.blen = 0x0000; // BSS size (not used) TODO: properly implement BSS
    header16.stack = 0x0000; // unknown (need to calc later)

    layout.header = reinterpret_cast<O65_Header_16*>(&header16);

    if (addVectorTable) {
        static implementation_defined::O65_File_Layout vectorLayout;
        vectorLayout = generateVectorTable();
        if (vectorLayout.header != nullptr) {
            layout.next_section = &vectorLayout;

            O65_Mode newMode = layout.mode;
            newMode.chain = ChainFlag::CHAINED;
            layout.mode = newMode;
            header16.mode = layout.mode.encode();
        }
    } else {
        layout.next_section = nullptr; // No idk what else it'd be rn
    }
    
    layout.text_data = textSegment.data();
    layout.data_data = dataSegment.data();

    auto undef = writeUndefinedReferences();
    auto globals = writeExportedGlobals();
    auto text_reloc = writeRelocationTable(textRelocs, header16.tbase);
    auto data_reloc = writeRelocationTable(dataRelocs, header16.dbase); // seriously, why tf was this passing textRelocs

    layout.undef_refs = implementation_defined::O65_Table_View{undef.data(), static_cast<uint32_t>(undef.size())};
    layout.text_reloc = implementation_defined::O65_Table_View{text_reloc.data(), static_cast<uint32_t>(text_reloc.size())};
    layout.data_reloc = implementation_defined::O65_Table_View{data_reloc.data(), static_cast<uint32_t>(data_reloc.size())};
    layout.exported_globals = implementation_defined::O65_Table_View{globals.data(), static_cast<uint32_t>(globals.size())};

    

    // Build header options
    auto headerOptions = buildHeaderOptions();

    // Serialize manually to include header options
    std::vector<uint8_t> o65Data;

    // Serialize fixed header
    // const uint8_t* headerPtr = reinterpret_cast<const uint8_t*>(&header16);
    // Use serializeO65 for serialization
    layout.header = &header16;
    
    // Store vectors in temporary storage so they remain valid during serialization
    std::vector<uint8_t> headerOptionsStorage = headerOptions;
    
    o65Data = serializeO65(layout);
    
    // Insert header options after the fixed header (before text segment)
    // The serializeO65 function puts a single terminator, we need to replace it with our options
    // Find where header ends and insert our options
    size_t headerEnd = sizeof(O65_Header_16);
    
    // Remove the single terminator that serializeO65 added and insert our full options
    o65Data.erase(o65Data.begin() + headerEnd);
    o65Data.insert(o65Data.begin() + headerEnd, headerOptions.begin(), headerOptions.end());

    return o65Data;
}

implementation_defined::O65_File_Layout CodegenVisitor::generateVectorTable()
{
    static implementation_defined::O65_File_Layout layout = {};
    static O65_Header_16 header = {};
    static std::vector<uint8_t> textData;
    static std::vector<uint8_t> undef_bytes;
    static std::vector<uint8_t> reloc_bytes;
    static std::vector<uint8_t> empty_reloc;
    static std::vector<uint8_t> empty_globals;
    static bool initialized = false;
    
    if (!initialized) {
        initialized = true;
        
        // Initialize layout to safe defaults
        layout.header = nullptr;
        layout.text_data = nullptr;
        layout.data_data = nullptr;
        layout.undef_refs = {nullptr, 0};
        layout.text_reloc = {nullptr, 0};
        layout.data_reloc = {nullptr, 0};
        layout.exported_globals = {nullptr, 0};
        layout.next_section = nullptr;
        
        uint8_t needed = 0x00;
        if (nmiHandler.has_value()) needed |= 0x01;
        if (resetHandler.has_value()) needed |= 0x02;
        if (irqHandler.has_value()) needed |= 0x04;
        
        // Just export the symbols
        if (nmiHandler.has_value()) {
            auto sym = findSymbol(nmiHandler.value());
            if (sym.has_value()) {
                exportedGlobals.push_back({"_nmi_handler", sym->offset});
            }
        }
        
        if (resetHandler.has_value()) {
            auto sym = findSymbol(resetHandler.value());
            if (sym.has_value()) {
                exportedGlobals.push_back({"_reset_handler", sym->offset});
            }
        }

        if (irqHandler.has_value()) {
            auto sym = findSymbol(irqHandler.value());
            if (sym.has_value()) {
                exportedGlobals.push_back({"_irq_handler", sym->offset});
            }
        }

        uint8_t offset = 0;
        uint8_t size = 6;

        // Use logical NOT (!) instead of bitwise NOT (~) for boolean checks
        if (!(needed & 0x01)) {
            size -= 2;
        }
        // Only exclude the middle vector (RESET) if it doesn't create a gap 
        // between NMI and IRQ. i.e., at least one of NMI or IRQ must also be missing.
        if (!(needed & 0x02) && (!(needed & 0x01) || !(needed & 0x04))) {
            size -= 2;
        }
        if (!(needed & 0x04)) {
            size -= 2;
        }

        if (size == 0) {
            // No vectors needed, return empty layout
            layout.header = nullptr;
            return layout;
        }

        if (!(needed & 0x01) && !(needed & 0x02)) {
            offset = 4; // Skip NMI and RESET
        } else if (!(needed & 0x01)) {
            offset = 2; // Skip NMI
        }

        std::memcpy(header.marker, o65::NON_C64_MARKER, 2);
        std::memcpy(header.magic, o65::MAGIC_NUMBER, 3);
        header.version = 0x00;
        header.mode = getMode().encode();
        header.tbase = 0xFFFA + offset; // Vector table base address
        header.tlen = size;       // Number of vectors * 2 bytes
        header.dlen = 0;
        header.zlen = 0;
        header.stack = 0;

        // The text segment for this file is just the placeholder bytes for the vectors
        textData.resize(size, 0x00);
        
        std::vector<std::string> imports;
        std::vector<o65::O65_Relocation_Entry> relocs;

        // Helper to generate an import referencing the exported handlers from the main object
        auto addHandlerReloc = [&](const std::optional<std::string>& handler, const std::string& importName, uint32_t vecOffset) {
            if (handler.has_value()) {
                size_t idx = imports.size();
                imports.push_back(importName);

                o65::O65_Relocation_Entry entry;
                entry.offset_from_previous = 0;
                entry.absolute_offset = vecOffset;
                entry.typebyte = o65::make_typebyte(o65::RelocationType::WORD, o65::SegmentID::UNDEFINED);
                entry.symbol_index = static_cast<uint8_t>(idx);
                entry.low_byte = 0;
                entry.seg_offset = 0;
                
                relocs.push_back(entry);
            }
        };

        // NMI at offset 0 (0xFFFA), RESET at 2 (0xFFFC), IRQ at 4 (0xFFFE)
        if (needed & 0x01) addHandlerReloc(nmiHandler, "_nmi_handler", 0);
        if (needed & 0x02) addHandlerReloc(resetHandler, "_reset_handler", (needed & 0x01) ? 2 : 0);
        if (needed & 0x04) addHandlerReloc(irqHandler, "_irq_handler", size - 2);

        // Serialize the imports (undefined references) table
        size_t count = imports.size();
        undef_bytes.push_back(count & 0xFF);
        undef_bytes.push_back((count >> 8) & 0xFF);
        for (const auto& name : imports) {
            for (char c : name) undef_bytes.push_back(c);
            undef_bytes.push_back(0);
        }

        // Serialize relocation table
        // Pass 0 as base since absolute_offset in relocs is segment-relative
        reloc_bytes = writeRelocationTable(relocs, 0);
        
        // Create empty terminators for unused segments
        empty_reloc = {o65::RELOCATION_TABLE_TERMINATOR};
        empty_globals = {0x00, 0x00}; // Count = 0

        layout.header = &header;
        layout.mode = getMode();
        layout.text_data = textData.data();
        layout.data_data = nullptr;
        layout.undef_refs = {undef_bytes.data(), static_cast<uint32_t>(undef_bytes.size())};
        layout.text_reloc = {reloc_bytes.data(), static_cast<uint32_t>(reloc_bytes.size())};
        layout.data_reloc = {empty_reloc.data(), static_cast<uint32_t>(empty_reloc.size())};
        layout.exported_globals = {empty_globals.data(), static_cast<uint32_t>(empty_globals.size())};
        layout.next_section = nullptr;
    }
    
    return layout;
}

// ============================================================================
// MULTIPLICATION ROUTINES
// ============================================================================

void CodegenVisitor::emitMultiply8Shift()
{
    // 8-bit multiply using shift-and-add algorithm
    // Input: A = multiplier, __temporary = multiplicand
    // Output: A = low byte, X = high byte of 16-bit result
    // Clobbers: Y, temp locations
    
    auto multiplicand = getZeroPageAllocation("__temporary").value().address;
    auto resultLo = getZeroPageAllocation("__temporary_2p").value().address;
    auto resultHi = resultLo + 1;
    
    // Initialize result to 0
    emitOp(Opcodes::LDX_IMMEDIATE);
    emit(0x00);
    emitOp(Opcodes::STX_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STX_ZEROPAGE);
    emit(resultHi);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Save multiplier to Y for counting
    emitOp(Opcodes::TAY);
    
    // Loop 8 times (for 8-bit multiply)
    emitOp(Opcodes::LDX_IMMEDIATE);
    emit(8);
    
    auto loopStart = createLabel("mul_loop");
    auto skipAdd = createLabel("mul_skip");
    auto loopEnd = createLabel("mul_end");
    
    emitLabel(loopStart);
    
    // Check if LSB of multiplier is set
    emitOp(Opcodes::TYA);
    emitOp(Opcodes::AND_IMMEDIATE);
    emit(0x01);
    emitJump(Opcodes::BEQ, skipAdd);
    
    // Add multiplicand to result
    emitOp(Opcodes::CLC);
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ADC_ZEROPAGE);
    emit(multiplicand);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(resultHi);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ADC_IMMEDIATE);
    emit(0x00);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(resultHi);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitLabel(skipAdd);
    
    // Shift multiplicand left
    emitOp(Opcodes::ASL_ZEROPAGE);
    emit(multiplicand);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Shift multiplier right
    emitOp(Opcodes::TYA);
    emitOp(Opcodes::LSR_ACCUMULATOR);
    emitOp(Opcodes::TAY);
    
    // Decrement counter and loop
    emitOp(Opcodes::DEX);
    emitJump(Opcodes::BNE, loopStart);
    
    emitLabel(loopEnd);
    
    // Load result into A (low) and X (high)
    emitOp(Opcodes::LDX_ZEROPAGE);
    emit(resultHi);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
}

void CodegenVisitor::emitMultiply8Table()
{
    // 8-bit multiply using (a*b) = sq[a+b] - sq[|a-b|]
    // where sq[n] = floor(n²/4)
    // This requires a 512-byte lookup table for sq[0..511]
    
    // Embed the table if not already done
    if (!squaresTableEmbedded) {
        squaresTableOffset = embedSquaresTable();
        squaresTableEmbedded = true;
    }
    
    auto multiplicand = getZeroPageAllocation("__temporary").value().address;
    auto temp = getZeroPageAllocation("__temporary_dos").value().address;
    auto resultLo = getZeroPageAllocation("__temporary_2p").value().address;
    auto resultHi = resultLo + 1;
    auto sumIdx = temp;
    auto diffIdx = temp + 1;
    
    // Save A (multiplier) to temp
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(temp);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Compute sum = A + multiplicand (for table index)
    emitOp(Opcodes::CLC);
    emitOp(Opcodes::ADC_ZEROPAGE);
    emit(multiplicand);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(sumIdx);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Compute diff = |A - multiplicand|
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(temp);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::SEC);
    emitOp(Opcodes::SBC_ZEROPAGE);
    emit(multiplicand);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Branch if result is positive (A >= multiplicand)
    auto noNegate = createLabel("mul_no_neg");
    emitJump(Opcodes::BPL, noNegate);
    
    // Negate if negative: result = 0 - result
    emitOp(Opcodes::EOR_IMMEDIATE);
    emit(0xFF);
    emitOp(Opcodes::CLC);
    emitOp(Opcodes::ADC_IMMEDIATE);
    emit(0x01);
    
    emitLabel(noNegate);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(diffIdx);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Load sq[sum] low byte - table address + sumIdx
    emitOp(Opcodes::LDY_ZEROPAGE);
    emit(sumIdx);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::LDA_ABSOLUTE_Y);
    emit(static_cast<uint8_t>(squaresTableOffset & 0xFF));
    emit(static_cast<uint8_t>((squaresTableOffset >> 8) & 0xFF));
    addTextReloc(RelocationType::WORD, SegmentID::DATA);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Load sq[sum] high byte (table + 256 + sumIdx)
    emitOp(Opcodes::LDA_ABSOLUTE_Y);
    emit(static_cast<uint8_t>((squaresTableOffset + 256) & 0xFF));
    emit(static_cast<uint8_t>(((squaresTableOffset + 256) >> 8) & 0xFF));
    addTextReloc(RelocationType::WORD, SegmentID::DATA);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(resultHi);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Subtract sq[diff] from result
    emitOp(Opcodes::LDY_ZEROPAGE);
    emit(diffIdx);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // resultLo -= sqLo[diff]
    emitOp(Opcodes::SEC);
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::SBC_ABSOLUTE_Y);
    emit(static_cast<uint8_t>(squaresTableOffset & 0xFF));
    emit(static_cast<uint8_t>((squaresTableOffset >> 8) & 0xFF));
    addTextReloc(RelocationType::WORD, SegmentID::DATA);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // resultHi -= sqHi[diff] - borrow
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(resultHi);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::SBC_ABSOLUTE_Y);
    emit(static_cast<uint8_t>((squaresTableOffset + 256) & 0xFF));
    emit(static_cast<uint8_t>(((squaresTableOffset + 256) >> 8) & 0xFF));
    addTextReloc(RelocationType::WORD, SegmentID::DATA);
    emitOp(Opcodes::TAX);
    
    // Load result into A (low) and X (high)
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(resultLo);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
}

size_t CodegenVisitor::embedSquaresTable()
{
    // Generate sq[n] = floor(n²/4) for n = 0..511
    // Split into low and high byte tables for easier indexing
    // Table layout: sqLo[0..255], sqLo[256..511], sqHi[0..255], sqHi[256..511]
    
    size_t tableStart = dataSegment.size();
    
    // Generate sqLo table (low bytes of n²/4 for n=0..511)
    for (int n = 0; n < 512; ++n) {
        uint16_t sq = static_cast<uint16_t>((n * n) / 4);
        emitData(static_cast<uint8_t>(sq & 0xFF));
    }
    
    // Generate sqHi table (high bytes of n²/4 for n=0..511)
    for (int n = 0; n < 512; ++n) {
        uint16_t sq = static_cast<uint16_t>((n * n) / 4);
        emitData(static_cast<uint8_t>((sq >> 8) & 0xFF));
    }
    
    return tableStart;
}

void CodegenVisitor::emitMultiply16Shift()
{
    // 16-bit multiply using shift-and-add algorithm
    // Input: __temporary (2 bytes) = multiplier, __temporary_2p (2 bytes) = multiplicand
    // Output: __temporary_2p (4 bytes) = 32-bit result (low to high)
    
    auto multiplier = getZeroPageAllocation("__temporary").value().address;
    auto multiplicand = getZeroPageAllocation("__temporary_2p").value().address;
    auto result = getZeroPageAllocation("__temporary_dos").value().address; // Need 4 bytes for result
    
    // We'll use __temporary_dos as 4-byte result, then copy back
    // Initialize result to 0
    emitOp(Opcodes::LDA_IMMEDIATE);
    emit(0x00);
    for (int i = 0; i < 4; ++i) {
        emitOp(Opcodes::STA_ZEROPAGE);
        emit(result + i);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    }
    
    // Loop 16 times
    emitOp(Opcodes::LDX_IMMEDIATE);
    emit(16);
    
    auto loopStart = createLabel("mul16_loop");
    auto skipAdd = createLabel("mul16_skip");
    
    emitLabel(loopStart);
    
    // Check if LSB of multiplier is set
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(multiplier);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::AND_IMMEDIATE);
    emit(0x01);
    emitJump(Opcodes::BEQ, skipAdd);
    
    // Add multiplicand to result (32-bit add)
    emitOp(Opcodes::CLC);
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(result);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ADC_ZEROPAGE);
    emit(multiplicand);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(result);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(result + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ADC_ZEROPAGE);
    emit(multiplicand + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(result + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(result + 2);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ADC_IMMEDIATE);
    emit(0x00);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(result + 2);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(result + 3);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ADC_IMMEDIATE);
    emit(0x00);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(result + 3);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitLabel(skipAdd);
    
    // Shift multiplicand left by 1 (32-bit shift)
    emitOp(Opcodes::ASL_ZEROPAGE);
    emit(multiplicand);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ROL_ZEROPAGE);
    emit(multiplicand + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Shift multiplier right by 1 (16-bit shift)
    emitOp(Opcodes::LSR_ZEROPAGE);
    emit(multiplier + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ROR_ZEROPAGE);
    emit(multiplier);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Decrement counter and loop
    emitOp(Opcodes::DEX);
    emitJump(Opcodes::BNE, loopStart);
    
    // Copy result back to __temporary_2p (just low 16 bits for now)
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(result);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(multiplicand);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(result + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(multiplicand + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Load result into A (low) and X (high)
    emitOp(Opcodes::LDX_ZEROPAGE);
    emit(result + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(result);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
}

void CodegenVisitor::emitDivide16()
{
    // 16-bit divide using binary long division
    // Input: __temporary (2 bytes) = dividend, __temporary_2p (2 bytes) = divisor
    // Output: __temporary = quotient, __temporary_2p = remainder
    // Algorithm: For each bit, shift remainder left, bring in next dividend bit,
    //            compare to divisor, subtract if >=, shift result bit into quotient
    
    auto dividend = getZeroPageAllocation("__temporary").value().address;
    auto divisor = getZeroPageAllocation("__temporary_2p").value().address;
    auto quotient = getZeroPageAllocation("__temporary_dos").value().address;
    auto remainder = quotient + 2;
    
    // Initialize quotient and remainder to 0
    emitOp(Opcodes::LDA_IMMEDIATE);
    emit(0x00);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(quotient);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(quotient + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(remainder + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Loop 16 times (for 16-bit division)
    emitOp(Opcodes::LDX_IMMEDIATE);
    emit(16);
    
    auto loopStart = createLabel("div16_loop");
    auto doSubtract = createLabel("div16_sub");
    auto afterCheck = createLabel("div16_after");
    
    emitLabel(loopStart);
    
    // Shift quotient left by 1 (to make room for new bit)
    emitOp(Opcodes::ASL_ZEROPAGE);
    emit(quotient);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ROL_ZEROPAGE);
    emit(quotient + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Shift remainder left by 1, bringing in MSB of dividend
    emitOp(Opcodes::ASL_ZEROPAGE);
    emit(dividend);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ROL_ZEROPAGE);
    emit(dividend + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ROL_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::ROL_ZEROPAGE);
    emit(remainder + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Compare remainder >= divisor (16-bit comparison)
    // Compare high bytes first
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(remainder + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::CMP_ZEROPAGE);
    emit(divisor + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitJump(Opcodes::BCC, afterCheck); // remainder < divisor (high byte less)
    emitJump(Opcodes::BNE, doSubtract); // remainder > divisor (high byte greater)
    
    // High bytes equal, check low bytes
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::CMP_ZEROPAGE);
    emit(divisor);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitJump(Opcodes::BCC, afterCheck); // remainder < divisor
    
    emitLabel(doSubtract);
    
    // remainder >= divisor: subtract and set quotient bit
    emitOp(Opcodes::SEC);
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::SBC_ZEROPAGE);
    emit(divisor);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(remainder + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::SBC_ZEROPAGE);
    emit(divisor + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(remainder + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Set LSB of quotient (we already shifted, so just INC)
    emitOp(Opcodes::INC_ZEROPAGE);
    emit(quotient);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitLabel(afterCheck);
    
    // Decrement counter and loop
    emitOp(Opcodes::DEX);
    emitJump(Opcodes::BNE, loopStart);
    
    // Copy quotient to __temporary
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(quotient);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(dividend);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(quotient + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(dividend + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Copy remainder to __temporary_2p
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(divisor);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(remainder + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(divisor + 1);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
}

void CodegenVisitor::emitDivide8()
{
    // 8-bit divide using repeated subtraction
    // Input: A = dividend, __temporary = divisor
    // Output: A = quotient, X = remainder
    
    auto divisor = getZeroPageAllocation("__temporary").value().address;
    auto quotient = getZeroPageAllocation("__temporary_2p").value().address;
    auto remainder = quotient + 1;
    
    // Initialize quotient to 0
    emitOp(Opcodes::LDX_IMMEDIATE);
    emit(0x00);
    emitOp(Opcodes::STX_ZEROPAGE);
    emit(quotient);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // Store dividend as initial remainder
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    auto loopStart = createLabel("div_loop");
    auto loopEnd = createLabel("div_end");
    
    emitLabel(loopStart);
    
    // Check if remainder >= divisor
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::CMP_ZEROPAGE);
    emit(divisor);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitJump(Opcodes::BCC, loopEnd); // remainder < divisor, done
    
    // remainder -= divisor
    emitOp(Opcodes::SEC);
    emitOp(Opcodes::SBC_ZEROPAGE);
    emit(divisor);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    // quotient++
    emitOp(Opcodes::INC_ZEROPAGE);
    emit(quotient);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    emitJump(Opcodes::JMP_ABSOLUTE, loopStart);
    
    emitLabel(loopEnd);
    
    // Load results: A = quotient, X = remainder
    emitOp(Opcodes::LDX_ZEROPAGE);
    emit(remainder);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    emitOp(Opcodes::LDA_ZEROPAGE);
    emit(quotient);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
}

void CodegenVisitor::loadStackVariable(int32_t offset, size_t size)
{
    auto fp = getZeroPageAllocation("__frame_pointer").value().address;
    auto temp = getZeroPageAllocation("__temporary_2p").value().address;
    
    if (offset >= 0) {
        // Positive offset (parameters)
        emitOp(Opcodes::LDY_IMMEDIATE);
        emit(static_cast<uint8_t>(offset));
        emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
        emit(fp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        if (size > 1) {
            emitOp(Opcodes::INY);
            emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
            emit(fp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            emitOp(Opcodes::TAX);
            emitOp(Opcodes::DEY);
            emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
            emit(fp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        }
    } else {
        // Negative offset (locals) - compute address
        emitOp(Opcodes::SEC);
        emitOp(Opcodes::LDA_ZEROPAGE);
        emit(fp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::SBC_IMMEDIATE);
        emit(static_cast<uint8_t>((-offset) & 0xFF));
        emitOp(Opcodes::STA_ZEROPAGE);
        emit(temp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        emitOp(Opcodes::LDA_ZEROPAGE);
        emit(fp + 1);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::SBC_IMMEDIATE);
        emit(0x00);
        emitOp(Opcodes::STA_ZEROPAGE);
        emit(temp + 1);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        emitOp(Opcodes::LDY_IMMEDIATE);
        emit(0x00);
        emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
        emit(temp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        if (size > 1) {
            emitOp(Opcodes::INY);
            emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
            emitOp(Opcodes::TAX);
            emitOp(Opcodes::DEY);
            emitOp(Opcodes::LDA_ZEROPAGE_INDIRECT_Y);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        }
    }
}

void CodegenVisitor::storeStackVariable(int32_t offset, size_t size)
{
    auto fp = getZeroPageAllocation("__frame_pointer").value().address;
    auto temp = getZeroPageAllocation("__temporary_2p").value().address;
    auto tempA = getZeroPageAllocation("__temporary").value().address;
    
    // Save A to temp first
    emitOp(Opcodes::STA_ZEROPAGE);
    emit(tempA);
    addTextReloc(RelocationType::LOW, SegmentID::ZERO);
    
    if (offset >= 0) {
        emitOp(Opcodes::LDY_IMMEDIATE);
        emit(static_cast<uint8_t>(offset));
        emitOp(Opcodes::LDA_ZEROPAGE);
        emit(tempA);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
        emit(fp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        if (size > 1) {
            emitOp(Opcodes::INY);
            emitOp(Opcodes::TXA);
            emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
            emit(fp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        }
    } else {
        // Compute address for negative offset
        emitOp(Opcodes::SEC);
        emitOp(Opcodes::LDA_ZEROPAGE);
        emit(fp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::SBC_IMMEDIATE);
        emit(static_cast<uint8_t>((-offset) & 0xFF));
        emitOp(Opcodes::STA_ZEROPAGE);
        emit(temp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        emitOp(Opcodes::LDA_ZEROPAGE);
        emit(fp + 1);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::SBC_IMMEDIATE);
        emit(0x00);
        emitOp(Opcodes::STA_ZEROPAGE);
        emit(temp + 1);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        emitOp(Opcodes::LDY_IMMEDIATE);
        emit(0x00);
        emitOp(Opcodes::LDA_ZEROPAGE);
        emit(tempA);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
        emit(temp);
        addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        
        if (size > 1) {
            emitOp(Opcodes::INY);
            emitOp(Opcodes::TXA);
            emitOp(Opcodes::STA_ZEROPAGE_INDIRECT_Y);
            emit(temp);
            addTextReloc(RelocationType::LOW, SegmentID::ZERO);
        }
    }
}

void CodegenVisitor::visit(ReturnNode &node)
{
    if (node.expression) {
        // Evaluate return expression
        node.expression->accept(*this);
        auto result = evalStack.top();
        evalStack.pop();
        
        // Load result into A (and X for 16-bit)
        loadIntoRegister(result);
    }
    
    // Emit function epilogue and return
    if (currentFunctionIsInterrupt) {
        // Restore registers for interrupt handler
        emitOp(Opcodes::PLY);
        emitOp(Opcodes::PLX);
        emitOp(Opcodes::PLA);
        emitOp(Opcodes::RTI);
    } else {
        emitEpilogue(0); // frameSize not used in current implementation
        emitOp(Opcodes::RTS);
    }
}

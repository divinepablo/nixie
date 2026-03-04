#include "linker.hpp"
#include <array>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <optional>
#include <argparse.hpp>

file_content_t read_file(const std::string& filename) {
    std::ifstream file(filename, std::ios::binary);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    auto content = buffer.str();
    file_content_t result = {};
    for (size_t i = 0; i < content.size(); ++i) {
        result[i] = static_cast<uint8_t>(content[i]);
    }
    return result;
}

int get_padding(int size, AlignmentMode mode) { // blame copilot if this doesnt work
    switch (mode) {
        case AlignmentMode::ALIGN_BYTE:
            return 0;
        case AlignmentMode::ALIGN_WORD:
            return (2 - (size % 2)) % 2;
        case AlignmentMode::ALIGN_LONG:
            return (4 - (size % 4)) % 4;
        case AlignmentMode::ALIGN_BLOCK:
            return (256 - (size % 256)) % 256;
        default:
            throw std::runtime_error("Invalid alignment mode");
    }
}

void Linker::load_files()
{
    for (const auto& filename : config.files) {
        this->load_object_file(filename);
    }
}

std::vector<uint8_t> Linker::link()
{
    std::vector<uint8_t> output;

    if (config.verbose) {
        std::cout << "Starting linking process...\n";
    }

    if (this->config.text_base < 0x0001) {
        this->config.text_base = 0x8000;
    }
    if (this->config.data_base < 0x0001) {
        this->config.data_base = 0x0200;
    }
    if (this->config.bss_base < 0x0001) {
        this->config.bss_base = this->config.data_base;
    }
    if (this->config.zero_base < 0x0001) {
        this->config.zero_base = 0x0000;
    }

    // 2. Decide base addresses for each segment
    // (for simplicity, we just use the provided base addresses or 0x0000 if not provided)
    uint16_t full_text_len = 0;
    uint16_t full_data_len = 0;
    uint16_t full_bss_len  = 0;
    uint16_t full_zero_len = 0;

    
    std::vector<uint8_t> full_data;
    std::vector<uint8_t> full_text;

        this->inject_symbol(ExportedGlobal{
        .name = "__DATA_SIZE__",
        .segment = SegmentID::ABSOLUTE,
        .value = full_data_len,
        .parent = nullptr
    });
    this->inject_symbol(ExportedGlobal{
        .name = "__DATA_SORC__",
        .segment = SegmentID::ABSOLUTE,
        .value = 0x0000, // we will set this to the correct value later, after we know the final text segment size and data segment size, since the data segment will be located immediately after the text segment in ROM
        .parent = nullptr
    });
    this->inject_symbol(ExportedGlobal{
        .name = "__DATA_DEST__",
        .segment = SegmentID::ABSOLUTE,
        .value = this->config.data_base,
        .parent = nullptr
    });

    for (auto &&object_file : object_files)
    {
        for (auto &&undefined : object_file.undefined_references)
        {
            if (!undefined.resolved)
            {
                auto existing = this->global_symbols[undefined.name];
                if (existing)
                    undefined.resolved = existing.value().second;
                else
                    throw std::runtime_error("Undefined symbol '" + undefined.name + "' not found in global symbol table");
            }
        }
        
        if (this->config.only_decide_zero_bases && object_file.header.tbase != 0x0000) {
            object_file.new_tbase = object_file.header.tbase;
        } else {
            auto pad = get_padding(full_text_len, object_file.mode.alignment);
            for (int i = 0; i < pad; ++i) {
                full_text.push_back(0x00);
            }
            full_text_len += pad;
            object_file.new_tbase = this->config.text_base + full_text_len;
            full_text_len += object_file.text_segment.size();
        }
        if (this->config.only_decide_zero_bases && object_file.header.dbase != 0x0000) {
            object_file.new_dbase = object_file.header.dbase;
        } else {
            auto pad = get_padding(full_data_len, object_file.mode.alignment);
            full_data_len += pad;
            object_file.new_dbase = this->config.data_base + full_data_len;
            full_data_len += object_file.data_segment.size();
        }
        if (this->config.only_decide_zero_bases && object_file.header.bbase != 0x0000) {
            object_file.new_bbase = object_file.header.bbase;
        } else {
            auto pad = get_padding(full_bss_len, object_file.mode.alignment);
            full_bss_len += pad;
            object_file.new_bbase = this->config.bss_base + full_bss_len;
            full_bss_len += object_file.header.blen;
        }
        if (this->config.only_decide_zero_bases && object_file.header.zbase != 0x0000) {
            object_file.new_zbase = object_file.header.zbase;
        } else {
            auto pad = get_padding(full_zero_len, object_file.mode.alignment);
            full_zero_len += pad;
            object_file.new_zbase = this->config.zero_base + full_zero_len;
            full_zero_len += object_file.header.zlen;
        }

        if (this->config.zero_base > 0x100) {
            throw std::runtime_error("Zero segment base address cannot be greater than 0x0100, since the zero segment is only used for zero-page addressing modes which can only address the first 256 bytes of memory");
        }

    }

    if (config.bss_base == config.data_base) {
        config.bss_base = config.data_base + full_data_len;
    }

    uint16_t data_rom_address = this->config.text_base + full_text_len;

    // this->global_symbols["__DATA_SIZE__"].value().second.value = full_data_len;
    // this->global_symbols["__DATA_SORC__"].value().second.value = data_rom_address;
    // this->global_symbols["__DATA_DEST__"].value().second.value = config.data_base;

    this->inject_symbol(ExportedGlobal{
        .name = "__DATA_SIZE__",
        .segment = SegmentID::ABSOLUTE,
        .value = full_data_len,
        .parent = nullptr
    });
    this->inject_symbol(ExportedGlobal{
        .name = "__DATA_SORC__",
        .segment = SegmentID::ABSOLUTE,
        .value = data_rom_address,
        .parent = nullptr
    });
    this->inject_symbol(ExportedGlobal{
        .name = "__DATA_DEST__",
        .segment = SegmentID::ABSOLUTE,
        .value = this->config.data_base,
        .parent = nullptr
    });

    uint16_t rom_end = this->config.text_base + full_text_len;
    for (auto &&object_file : object_files)
    {
        if (object_file.new_tbase + object_file.header.tlen > rom_end) {
            rom_end = object_file.new_tbase + object_file.header.tlen;
        }
    }

    if (data_rom_address + full_data_len > rom_end) {
        rom_end = data_rom_address + full_data_len;
    }

    uint32_t final_rom_size = rom_end - this->config.text_base;
    if (final_rom_size > 0x10000) {
        throw std::runtime_error("Final ROM size exceeds 64KB limit of 16-bit address space");
    }

    std::vector<uint8_t> final_rom;
    final_rom.resize(final_rom_size, 0x00);

    // Fix parent pointers for resolved undefined references so they
    // point at the actual ObjectFiles in object_files (which now have
    // their new base addresses assigned).
    for (auto& obj : object_files) {
        for (auto& undef : obj.undefined_references) {
            if (undef.resolved) {
                for (auto& candidate : object_files) {
                    for (auto& exp : candidate.exported_globals) {
                        if (exp.name == undef.resolved->name) {
                            undef.resolved->parent = &candidate;
                        }
                    }
                }
            }
        }
    }

    for (auto &&object_file : object_files)
    {
        this->apply_relocation_table(object_file.header.tbase, object_file.text_segment, object_file.text_relocations, object_file);
        this->apply_relocation_table(object_file.header.dbase, object_file.data_segment, object_file.data_relocations, object_file);

        // FIX: offset into ROM, not offset from old base
        uint16_t text_offset = object_file.new_tbase - this->config.text_base;
        for (size_t i = 0; i < object_file.text_segment.size(); ++i) {
            final_rom[text_offset + i] = object_file.text_segment[i];
        }
        for (auto &&i : object_file.data_segment)
        {
            full_data.push_back(i);
        }
    }

    if (this->config.fill && this->config.text_base > 0) {
        output.resize(this->config.text_base, 0x00);
    }
    output.insert(output.end(), final_rom.begin(), final_rom.end());
    
    return output;
}

ObjectFile Linker::parse_object_file(const std::string &filename, const file_content_t &content)
{
    file_content_t data = content;
    FileReader reader(data);
    auto object = ObjectFile(filename, reader.read<O65_Header_16>());
    if (object.header.is_valid()) {
        debug_log("Parsed header for {}", filename);
    } else {
        throw std::runtime_error("Invalid O65 header in file: " + filename + "(marker: " + std::to_string(object.header.marker[0]) + " " + std::to_string(object.header.marker[1]) + ", magic: " + std::to_string(object.header.magic[0]) + " " + std::to_string(object.header.magic[1]) + " " + std::to_string(object.header.magic[2]) + ")");

    }

    while (reader.peek_byte() != o65::HEADER_OPTIONS_TERMINATOR) {
        /* uint8_t option_type = */reader.read_byte();
        continue; // for now we just skip any options, since we dont support any, but in the future we can add support for options here
    }
    reader.read_byte(); // consume null terminator after options

    debug_log("Reading text segment for {}", filename);
    object.text_segment.resize(object.header.tlen);
    for (size_t i = 0; i < object.header.tlen; ++i) {
        object.text_segment[i] = reader.read_byte();
    }

    debug_log("Reading data segment for {}", filename);
    object.data_segment.resize(object.header.dlen);
    for (size_t i = 0; i < object.header.dlen; ++i) {
        object.data_segment[i] = reader.read_byte();
    }

    auto undefined_count = reader.read_word();
    debug_log("Reading {} undefined references for {}", undefined_count, filename);

    for (size_t i = 0; i < undefined_count; i++)
    {
        UndefinedReference ref;
        while (reader.peek_byte() != 0) {
            ref.name += static_cast<char>(reader.read_byte());
        }
        reader.read_byte(); // consume null terminator

        object.undefined_references.push_back(ref);
    }

    debug_log("Read {} undefined references for {}", object.undefined_references.size(), filename);

    parse_relocation_table(object.header.tbase, reader, object.text_relocations);
    debug_log("Read {} text relocations, at byte offset {}", object.text_relocations.size(), reader.tell());
    
    parse_relocation_table(object.header.dbase, reader, object.data_relocations);
    debug_log("Read {} data relocations, at byte offset {}", object.data_relocations.size(), reader.tell());
    
    uint16_t export_count = reader.read_word();
    debug_log("Reading {} exported globals for {}", export_count, filename);
    for (size_t i = 0; i < export_count; i++) {
        ExportedGlobal symbol;
        symbol.parent = &object;
        while (reader.peek_byte() != 0x00) {
            symbol.name += static_cast<char>(reader.read_byte());
        }
        reader.read_byte(); // consume null terminator
        symbol.segment = static_cast<o65::SegmentID>(reader.read_byte());
        symbol.value = reader.read_word();
        debug_log("Read exported symbol: '{}' at segment {} with value {}", symbol.name, static_cast<uint8_t>(symbol.segment), symbol.value);
        object.exported_globals.push_back(symbol);
        // this->global_symbols[symbol.name] = std::make_optional(std::make_pair(object, symbol));
    }
    debug_log("Read {} exported globals for {}", object.exported_globals.size(), filename);
    debug_log("{} total exported globals", this->global_symbols.size());
    debug_log("Finished parsing object file: {}", filename);

    if (object.mode.chain == ChainFlag::CHAINED) {
        auto without_chain = filename.find("$") == std::string::npos ? filename : filename.substr(0, filename.find("$"));
        debug_log("File {} is chained, loading next file in chain...", filename);
        file_content_t chained_data;
        std::copy(data.begin() + reader.tell(), data.end(), chained_data.begin()); // the chained file is just appended to the end of the current file, so we can just read from the current position to the end of the data buffer
        auto chained_object = parse_object_file(without_chain + "$" + std::to_string(++chain_count), chained_data);
        object.chain = new ObjectFile(std::move(chained_object));
    }
    return object;
}

void Linker::load_object_file(const std::string &filename)
{
    chain_count = 0; // reset chain count for each top-level file
    auto object = parse_object_file(filename, read_file(filename));



    ObjectFile* current = &object;
    while (current) {
        for (const auto& symbol : current->exported_globals) {
            if (global_symbols.count(symbol.name) > 0) {
                throw std::runtime_error("Duplicate symbol: " + symbol.name + " in file " + current->filename);
            }
            this->global_symbols[symbol.name] = std::make_optional(std::make_pair(*current, symbol));
        }
        if (config.verbose) {
            std::cout << "Loaded object file: " << current->filename << "\n";
        }
        current = current->chain;
    }

    current = &object;
    while (current) {
        for (auto& undefined : current->undefined_references) {
            if (global_symbols.count(undefined.name) > 0) {
                auto existing = global_symbols[undefined.name];
                if (existing.has_value()) {
                    debug_log("Resolved undefined reference: {} in file {} to symbol in file {}", undefined.name, current->filename, existing->first.filename);
                    undefined.resolved = existing.value().second;
                } else {
                    debug_log("Undefined reference: {} in file {} could not be resolved", undefined.name, current->filename);
                }
            } else {
                this->global_symbols[undefined.name] = std::nullopt;
                debug_log("Undefined reference: '{}' in file {} could not be resolved (added to global symbol table with nullopt)", undefined.name, current->filename);
            }
            
        }
        current = current->chain;
    }

    auto chained = object.chain;
    while (chained) {
        object_files.push_back(std::move(*chained));
        chained = chained->chain;
    }
    object_files.push_back(std::move(object));
}

void Linker::inject_symbol(ExportedGlobal symbol)
{
    uint16_t segment_base = 0;
    switch (symbol.segment) {
        case SegmentID::TEXT:
            segment_base = this->config.text_base;
            break;
        case SegmentID::DATA:
            segment_base = this->config.data_base;
            break;
        case SegmentID::BSS:
            segment_base = this->config.bss_base;
            break;
        case SegmentID::ZERO:
            segment_base = this->config.zero_base;
            break;
        case SegmentID::ABSOLUTE:
            segment_base = 0;
            break;
        default:
            throw std::runtime_error("Invalid segment for exported symbol: " + symbol.name);
    }

    symbol.value -= segment_base;

    ObjectFile new_symbol_file("injected:" + symbol.name, O65_Header_16{});
    new_symbol_file.mode = O65_Mode{
        .processor = ProcessorVariant::CPU_6502,
        .granularity = RelocationGranularity::BYTEWISE,
        .address_size = AddressSize::BITS_16,
        .file_type = FileType::OBJECT_FILE,
        .simple_mode = SimpleAddressing::SIMPLE,
        .chain = ChainFlag::STANDALONE,
        .bss_zero = BssZeroFlag::NO_REQUIREMENT,
        .cpu2_type = CPU2Type::CPU2_65C02,
        .alignment = AlignmentMode::ALIGN_BYTE
    };
    new_symbol_file.exported_globals.push_back(symbol);
    this->object_files.push_back(std::move(new_symbol_file));
    this->global_symbols[symbol.name] = std::make_optional(
        std::make_pair(this->object_files.back(), symbol)
    );
    debug_log("Injected symbol: {} at segment {} with value {}", symbol.name, static_cast<uint8_t>(symbol.segment), symbol.value);
}

void Linker::parse_relocation_table(uint16_t base, FileReader &reader, std::vector<O65_Relocation_Entry> &relocations)
{
    uint16_t last = base - 1;
    while (reader.peek_byte() != o65::RELOCATION_TABLE_TERMINATOR) {
        uint16_t offset_from_last = 0x00;
        while (reader.peek_byte() == 0xFF) {
            offset_from_last += 0xFE;
            reader.read_byte();
        }
        offset_from_last += reader.read_byte();
        uint16_t real_offset = last + offset_from_last;
        last = real_offset;

        uint8_t typebyte = reader.read_byte();

        O65_Relocation_Entry hi{};
        hi.offset_from_previous = offset_from_last;
        hi.absolute_offset = real_offset;
        hi.typebyte = typebyte;

        if (hi.is_undefined_reference()) {
            hi.symbol_index = reader.read_word();
        }

        if (hi.is_high_byte()) {
            hi.low_byte = reader.read_byte();
        }

        if (hi.is_segment_relocation()) {
            hi.seg_offset = reader.read_word();
        }

        debug_log("Added relocation at offset {} of type {} for segment {}", hi.absolute_offset, static_cast<uint8_t>(hi.type()), static_cast<uint8_t>(hi.segment()));
        relocations.push_back(hi);
    }
    reader.read_byte(); // consume terminator
}

void Linker::apply_relocation_table(uint16_t old_base, std::vector<uint8_t> &segment, const std::vector<O65_Relocation_Entry> &relocations, ObjectFile &object)
{
    for (auto &&reloc : relocations)
    {
        switch (reloc.type())
        {
        case RelocationType::LOW: {
            uint8_t original = segment.at(reloc.absolute_offset - old_base);
            uint8_t relocated;
            if (reloc.segment() != SegmentID::UNDEFINED) {
                relocated = relocate_low(original, static_cast<uint32_t>(object.header.get_base(reloc.segment())), static_cast<uint32_t>(object.get_new_base(reloc.segment())));
            } else {
                auto undefined = object.undefined_references.at(reloc.symbol_index);
                if (!undefined.resolved)
                    throw std::runtime_error("Undefined symbol '" + undefined.name + "' not resolved in relocation");
                relocated = undefined.resolved->value + undefined.resolved->parent->get_new_base(undefined.resolved->segment);
            }
            segment.at(reloc.absolute_offset - old_base) = relocated;
            break;
        }
        case RelocationType::HIGH: {
            uint8_t original = segment.at(reloc.absolute_offset - old_base);
            if (reloc.segment() != SegmentID::UNDEFINED) {
                uint8_t relocated = relocate_high(original, reloc.low_byte, static_cast<uint32_t>(object.header.get_base(reloc.segment())), static_cast<uint32_t>(object.get_new_base(reloc.segment())));
                segment.at(reloc.absolute_offset - old_base) = relocated;
            } else {
                auto undefined = object.undefined_references.at(reloc.symbol_index);
                if (!undefined.resolved)
                    throw std::runtime_error("Undefined symbol '" + undefined.name + "' not resolved in relocation");
                uint16_t exported = undefined.resolved->value + undefined.resolved->parent->get_new_base(undefined.resolved->segment);
                segment.at(reloc.absolute_offset - old_base) = static_cast<uint8_t>((exported >> 8) & 0xFF);
            }
            break;
        }
        case RelocationType::WORD: {
            uint8_t lo = segment.at(reloc.absolute_offset - old_base);
            uint8_t hi = segment.at(reloc.absolute_offset - old_base + 1);
            uint16_t original = (hi << 8) | lo ;
            uint16_t relocated;
            if (reloc.segment() != SegmentID::UNDEFINED) {
                relocated = relocate_word(original, static_cast<uint32_t>(object.header.get_base(reloc.segment())), static_cast<uint32_t>(object.get_new_base(reloc.segment())));
            } else {
                auto undefined = object.undefined_references.at(reloc.symbol_index);
                if (!undefined.resolved)
                    throw std::runtime_error("Undefined symbol '" + undefined.name + "' not resolved in relocation");
                relocated = undefined.resolved->value + undefined.resolved->parent->get_new_base(undefined.resolved->segment);
            }
            segment.at(reloc.absolute_offset - old_base) = relocated & 0xFF;
            segment.at(reloc.absolute_offset - old_base + 1) = (relocated & 0xFF00) >> 8;
            break;
        }
        
        default:
            break;
        }
    }
    
}

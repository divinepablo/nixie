#pragma once
#include <list>
#include <string>
#include <cstdint>
#include <map>
#include <optional>
#include "o65.hpp"
#include <vector>
#include <array>

// https://github.com/divinepablo/ranpy/blob/04df224d6e76b6dd8932dc2dc183380220bdea0b/o65linker.py#L55-L195
using namespace o65;

typedef std::array<uint8_t, 65536> file_content_t;

struct LinkerConfig {
    std::list<std::string> files;
    std::string output;
    bool raw_output = false;
    bool fill = false;
    bool verbose = false;
    bool only_decide_zero_bases = false;
    uint16_t text_base = 0x0000;
    uint16_t data_base = 0x0000;
    uint16_t bss_base = 0x0000;
    uint16_t zero_base = 0x0000;
};

struct ObjectFile;

struct ExportedGlobal {
    std::string name;
    o65::SegmentID segment;
    uint16_t value;
    ObjectFile* parent;
};

struct UndefinedReference {
    std::string name;
    std::optional<ExportedGlobal> resolved; // if resolved, contains the segment and value of the symbol
};

struct ObjectFile {
    std::string filename;
    O65_Header_16 header; // header 32 not real
    O65_Mode mode; // from header
    std::vector<uint8_t> text_segment;
    std::vector<uint8_t> data_segment;
    std::vector<UndefinedReference> undefined_references;
    std::vector<ExportedGlobal> exported_globals;
    std::vector<O65_Relocation_Entry> text_relocations;
    std::vector<O65_Relocation_Entry> data_relocations;
    std::vector<ExportedGlobal> exported;
    
    uint16_t new_tbase = 0x0000;
    uint16_t new_dbase = 0x0000;
    uint16_t new_bbase = 0x0000;
    uint16_t new_zbase = 0x0000;
    ObjectFile* chain = nullptr; // next file in chain, if any

    constexpr uint16_t get_new_base(SegmentID segment) {
        switch (segment)
        {
        case SegmentID::ZERO:
            return new_zbase;
        case SegmentID::TEXT:
            return new_tbase;
        case SegmentID::DATA:
            return new_dbase;
        case SegmentID::BSS:
            return new_bbase;
        default:
            return 0;
        }
    }

    ObjectFile(const std::string& filename, const O65_Header_16& header) : filename(filename), header(header), mode(O65_Mode::decode(header.mode)) {}
};

struct FileReader {
    uint16_t cursor = 0;
    file_content_t data;
    
    uint8_t read_byte() {
        return data[cursor++];
    }
    uint16_t read_word() {
        uint16_t low = read_byte();
        uint16_t high = read_byte();
        return (high << 8) | low;
    }

    uint8_t* read_bytes(size_t count) {
        uint8_t* ptr = &data[cursor];
        cursor += count;
        return ptr;
    }

    template <typename T>
    T read() {
        if constexpr (std::is_same_v<T, uint8_t>) {
            return read_byte();
        } else if constexpr (std::is_same_v<T, uint16_t>) {
            return read_word();
        } else {
            return *reinterpret_cast<T*>(read_bytes(sizeof(T)));
        }
    }

    uint16_t tell() const {
        return cursor;
    }

    uint16_t peek_word() const {
        uint16_t low = data[cursor];
        uint16_t high = data[cursor + 1];
        return (high << 8) | low;
    }

    uint8_t peek_byte() const {
        return data[cursor];
    }

    template <typename T>
    T peek() const {
        if constexpr (std::is_same_v<T, uint8_t>) {
            return peek_byte();
        } else if constexpr (std::is_same_v<T, uint16_t>) {
            return peek_word();
        } else {
            return *reinterpret_cast<const T*>(&data[cursor]);
        }
    }

    FileReader(const file_content_t& data) : data(data) {}
};
class Linker {
public:
    Linker(const LinkerConfig& config) : config(config) {};
    void load_files();
    std::vector<uint8_t> link();
    #define debug_log(msg) do { if (config.verbose) std::cout << msg << std::endl; } while(0)
private:
    LinkerConfig config;
    std::vector<ObjectFile> object_files;
    std::map<std::string, std::optional<std::pair<ObjectFile, ExportedGlobal>>> global_symbols; // all exported symbols from all files, for easy lookup during relocation
    ObjectFile parse_object_file(const std::string& filename, const file_content_t& content);
    void load_object_file(const std::string& filename);
    void inject_symbol(ExportedGlobal symbol);
    void parse_relocation_table(uint16_t base, FileReader& reader, std::vector<O65_Relocation_Entry>& relocations);
    void apply_relocation_table(uint16_t old_base, std::vector<uint8_t>& segment, const std::vector<O65_Relocation_Entry>& relocations, ObjectFile& object);
    // void parse_relocation_table(uint16 base, uint16 cursor, const std::vector<O65_Relocation_Entry>& entries)
    int chain_count = 0;

};

int get_padding(int size, AlignmentMode mode);
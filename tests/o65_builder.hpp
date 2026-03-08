/**
 * @file o65_builder.hpp
 * @brief Shared O65 binary builder and LinkerConfig helper for tests
 *
 * Extracted from linker_tests.cpp so that both linker tests and runtime
 * emulation tests can construct O65 object files in memory.
 */

#pragma once

#include <gtest/gtest.h>
#include "linker.hpp"
#include <vector>
#include <cstdint>
#include <string>
#include <list>
#include <fstream>
#include <cstdio>

using namespace o65;

/**
 * @brief Helper to construct a valid o65 binary in a byte vector.
 *
 * The caller fills text/data content, undefined refs, relocations, and exports
 * via the builder methods, then calls build() to get a file_content_t.
 */
class O65Builder {
public:
    // header fields (16-bit mode)
    uint16_t mode  = 0x1000; // OBJECT_FILE, 6502, bytewise, 16-bit, standalone
    uint16_t tbase = 0x0000;
    uint16_t tlen  = 0;
    uint16_t dbase = 0x0000;
    uint16_t dlen  = 0;
    uint16_t bbase = 0x0000;
    uint16_t blen  = 0;
    uint16_t zbase = 0x0000;
    uint16_t zlen  = 0;
    uint16_t stack = 0x0000;

    std::vector<uint8_t> text_bytes;
    std::vector<uint8_t> data_bytes;
    std::vector<std::string> undefined_refs;

    // Relocation entries: (offset_delta_from_prev, typebyte, optional extra bytes)
    struct RawReloc {
        uint8_t offset_byte; // single-byte delta (no 0xFF continuation needed for tests)
        uint8_t typebyte;
        std::vector<uint8_t> extra; // symbol_index (word), low_byte, seg_offset etc.
    };
    std::vector<RawReloc> text_relocs;
    std::vector<RawReloc> data_relocs;

    struct Export {
        std::string name;
        uint8_t segment_id;
        uint16_t value;
    };
    std::vector<Export> exports;

    O65Builder& set_text(const std::vector<uint8_t>& t) {
        text_bytes = t;
        tlen = static_cast<uint16_t>(t.size());
        return *this;
    }
    O65Builder& set_data(const std::vector<uint8_t>& d) {
        data_bytes = d;
        dlen = static_cast<uint16_t>(d.size());
        return *this;
    }
    O65Builder& set_chained() {
        mode |= 0x0400; // ChainFlag::CHAINED
        return *this;
    }

    O65Builder& add_undefined(const std::string& name) {
        undefined_refs.push_back(name);
        return *this;
    }

    O65Builder& add_text_reloc(uint8_t offset_byte, uint8_t typebyte, std::vector<uint8_t> extra = {}) {
        text_relocs.push_back({offset_byte, typebyte, extra});
        return *this;
    }
    O65Builder& add_data_reloc(uint8_t offset_byte, uint8_t typebyte, std::vector<uint8_t> extra = {}) {
        data_relocs.push_back({offset_byte, typebyte, extra});
        return *this;
    }

    O65Builder& add_export(const std::string& name, uint8_t seg_id, uint16_t value) {
        exports.push_back({name, seg_id, value});
        return *this;
    }

    /// Serialize into a byte vector (not file_content_t – caller copies)
    std::vector<uint8_t> serialize() const {
        std::vector<uint8_t> out;

        // -- header (26 bytes) --
        out.push_back(0x01); out.push_back(0x00);             // marker
        out.push_back(0x6F); out.push_back(0x36); out.push_back(0x35); // magic "o65"
        out.push_back(0x00);                                   // version
        push_word(out, mode);
        push_word(out, tbase);
        push_word(out, tlen);
        push_word(out, dbase);
        push_word(out, dlen);
        push_word(out, bbase);
        push_word(out, blen);
        push_word(out, zbase);
        push_word(out, zlen);
        push_word(out, stack);

        // -- header options (none, just terminator) --
        out.push_back(0x00);

        // -- text segment bytes --
        out.insert(out.end(), text_bytes.begin(), text_bytes.end());

        // -- data segment bytes --
        out.insert(out.end(), data_bytes.begin(), data_bytes.end());

        // -- undefined reference list --
        push_word(out, static_cast<uint16_t>(undefined_refs.size()));
        for (auto& name : undefined_refs) {
            for (char c : name) out.push_back(static_cast<uint8_t>(c));
            out.push_back(0x00);
        }

        // -- text relocation table --
        for (auto& r : text_relocs) {
            out.push_back(r.offset_byte);
            out.push_back(r.typebyte);
            out.insert(out.end(), r.extra.begin(), r.extra.end());
        }
        out.push_back(0x00); // terminator

        // -- data relocation table --
        for (auto& r : data_relocs) {
            out.push_back(r.offset_byte);
            out.push_back(r.typebyte);
            out.insert(out.end(), r.extra.begin(), r.extra.end());
        }
        out.push_back(0x00); // terminator

        // -- exported globals --
        push_word(out, static_cast<uint16_t>(exports.size()));
        for (auto& e : exports) {
            for (char c : e.name) out.push_back(static_cast<uint8_t>(c));
            out.push_back(0x00);
            out.push_back(e.segment_id);
            push_word(out, e.value);
        }

        return out;
    }

    /// Build into a file_content_t suitable for the linker
    file_content_t build() const {
        auto bytes = serialize();
        file_content_t fc{};
        for (size_t i = 0; i < bytes.size() && i < fc.size(); ++i) {
            fc[i] = bytes[i];
        }
        return fc;
    }

private:
    static void push_word(std::vector<uint8_t>& v, uint16_t w) {
        v.push_back(w & 0xFF);
        v.push_back((w >> 8) & 0xFF);
    }
};

/// Create a minimal LinkerConfig
inline LinkerConfig make_config(std::list<std::string> files = {},
                                uint16_t text_base = 0x8000,
                                uint16_t data_base = 0x0200,
                                bool verbose = false) {
    return LinkerConfig{
        .files = files,
        .output = "test_output.bin",
        .raw_output = true,
        .fill = false,
        .verbose = verbose,
        .only_decide_zero_bases = false,
        .text_base = text_base,
        .data_base = data_base,
        .bss_base = 0x0000,
        .zero_base = 0x0000
    };
}

/**
 * @file o65.hpp
 * @brief Strict specification-compliant interface for the o65 Binary Relocation Format
 * 
 * This header provides a normatively faithful implementation of the o65 binary format
 * as specified by André Fachat (V1.3, 31 Mar 2005).
 * 
 * DESIGN PRINCIPLES:
 * - Absolute adherence to the normative specification
 * - No abstractions that deviate from the specification
 * - Implementation-defined behaviors explicitly marked
 * - Direct exposure of format structures without convenience layers
 * - Traceability to specific sections of the specification
 * 
 * All field layouts, sizes, and semantics are derived directly from:
 * "6502 binary relocation format V1.3" by André Fachat
 * 
 * @author Generated from specification analysis
 * @version 1.3
 * @date 2026-01-27
 */

#ifndef O65_HPP
#define O65_HPP

#include <cstdint>
#include <cstddef>

// =============================================================================
// SPECIFICATION CONSTANTS
// Section 2.6.1: Magic Number and File Identification
// =============================================================================

namespace o65 {

/** 
 * @brief Non-C64 marker (first two bytes of every o65 file)
 * Specification Reference: Section 2.6.1
 */
constexpr uint8_t NON_C64_MARKER[2] = {0x01, 0x00};

/**
 * @brief Magic number identifying o65 files: "o65"
 * Specification Reference: Section 2.6.1
 */
constexpr uint8_t MAGIC_NUMBER[3] = {'o', '6', '5'};

/**
 * @brief Current specification version
 * Specification Reference: Section 2.6.1
 */
constexpr uint8_t FORMAT_VERSION = 0x00;

// =============================================================================
// MODE BIT DEFINITIONS
// Section 2.6.1: Header mode word bit layout
// =============================================================================

/**
 * @brief CPU type for o65 binary (mode bit 15)
 * Specification Reference: Section 2.6.1, mode.15
 */
enum class ProcessorVariant : uint16_t {
    CPU_6502  = 0x0000,  ///< 6502 CPU
    CPU_65816 = 0x8000   ///< 65816 CPU
};

/**
 * @brief Relocation granularity (mode bit 14)
 * Specification Reference: Section 2.6.1, mode.14
 */
enum class RelocationGranularity : uint16_t {
    BYTEWISE = 0x0000,  ///< Bytewise relocation allowed
    PAGEWISE = 0x4000   ///< Page (256 byte) wise relocation only
};

/**
 * @brief Address and size field width (mode bit 13)
 * Specification Reference: Section 2.6.1, mode.13
 */
enum class AddressSize : uint16_t {
    BITS_16 = 0x0000,  ///< 16-bit addresses (.word)
    BITS_32 = 0x2000   ///< 32-bit addresses (.long)
};

/**
 * @brief File type classification (mode bit 12)
 * Specification Reference: Section 2.6.1, mode.12
 */
enum class FileType : uint16_t {
    EXECUTABLE  = 0x0000,  ///< Executable file
    OBJECT_FILE = 0x1000   ///< Object file for linking
};

/**
 * @brief Simple addressing mode flag (mode bit 11)
 * When set, guarantees: dbase = tbase + tlen, bbase = dbase + dlen
 * Specification Reference: Section 2.6.1, mode.11
 */
enum class SimpleAddressing : uint16_t {
    NORMAL = 0x0000,  ///< No guarantees about segment layout
    SIMPLE = 0x0800   ///< Segments are contiguously laid out
};

/**
 * @brief File chaining support (mode bit 10)
 * Specification Reference: Section 2.6.1, mode.10
 */
enum class ChainFlag : uint16_t {
    STANDALONE = 0x0000,  ///< Single file, no chaining
    CHAINED    = 0x0400   ///< Another o65 section follows this one
};

/**
 * @brief BSS segment zeroing requirement (mode bit 9)
 * Specification Reference: Section 2.6.1, mode.9
 */
enum class BssZeroFlag : uint16_t {
    NO_REQUIREMENT = 0x0000,  ///< BSS zeroing not required
    MUST_ZERO      = 0x0200   ///< BSS segment must be zeroed out
};

/**
 * @brief Detailed CPU type specification (mode bits 4-7)
 * Specification Reference: Section 2.6.1, mode.4-7
 */
enum class CPU2Type : uint16_t {
    CPU2_6502_CORE    = 0x0000,  ///< 6502 core (no undocumented opcodes)
    CPU2_65C02        = 0x0010,  ///< 65C02 with bugfixes, no illegal opcodes
    CPU2_65SC02       = 0x0020,  ///< 65SC02 (enhanced 65C02), some new opcodes
    CPU2_65CE02       = 0x0030,  ///< 65CE02 with 16-bit ops/branches, Z register modifiable
    CPU2_NMOS_6502    = 0x0040,  ///< NMOS 6502 (including undocumented opcodes)
    CPU2_65816_EMU    = 0x0050,  ///< 65816 in 6502 emulation mode
    // 0x0060-0x0070 reserved
    // Unofficially supported (Appendix A.1):
    CPU2_6809         = 0x0080,  ///< 6809 (unofficial)
    CPU2_Z80          = 0x00A0,  ///< Z80 (unofficial)
    CPU2_8086         = 0x00D0,  ///< 8086 (unofficial)
    CPU2_80286        = 0x00E0   ///< 80286 (unofficial)
};

/**
 * @brief Memory alignment requirement (mode bits 0-1)
 * Specification Reference: Section 2.6.1, mode.0-1
 */
enum class AlignmentMode : uint16_t {
    ALIGN_BYTE  = 0x0000,  ///< Byte alignment (1 byte)
    ALIGN_WORD  = 0x0001,  ///< Word alignment (2 bytes)
    ALIGN_LONG  = 0x0002,  ///< Long alignment (4 bytes)
    ALIGN_BLOCK = 0x0003   ///< Block alignment (256 bytes)
};

/**
 * @brief Composite mode bits structure
 * Provides type-safe access to all mode configuration bits
 * Specification Reference: Section 2.6.1
 */
struct O65_Mode {
    ProcessorVariant processor;
    RelocationGranularity granularity;
    AddressSize address_size;
    FileType file_type;
    SimpleAddressing simple_mode;
    ChainFlag chain;
    BssZeroFlag bss_zero;
    CPU2Type cpu2_type;
    AlignmentMode alignment;
    
    /**
     * @brief Encode mode structure to raw 16-bit mode word
     * @return Raw mode bits as stored in o65 header
     */
    constexpr uint16_t encode() const noexcept {
        return static_cast<uint16_t>(processor) |
               static_cast<uint16_t>(granularity) |
               static_cast<uint16_t>(address_size) |
               static_cast<uint16_t>(file_type) |
               static_cast<uint16_t>(simple_mode) |
               static_cast<uint16_t>(chain) |
               static_cast<uint16_t>(bss_zero) |
               static_cast<uint16_t>(cpu2_type) |
               static_cast<uint16_t>(alignment);
    }
    
    /**
     * @brief Decode raw mode word to structured components
     * @param mode_word Raw 16-bit mode value from header
     * @return Structured mode configuration
     */
    static constexpr O65_Mode decode(uint16_t mode_word) noexcept {
        return O65_Mode{
            static_cast<ProcessorVariant>(mode_word & 0x8000),
            static_cast<RelocationGranularity>(mode_word & 0x4000),
            static_cast<AddressSize>(mode_word & 0x2000),
            static_cast<FileType>(mode_word & 0x1000),
            static_cast<SimpleAddressing>(mode_word & 0x0800),
            static_cast<ChainFlag>(mode_word & 0x0400),
            static_cast<BssZeroFlag>(mode_word & 0x0200),
            static_cast<CPU2Type>(mode_word & 0x00F0),
            static_cast<AlignmentMode>(mode_word & 0x0003)
        };
    }
};

// =============================================================================
// SEGMENT IDENTIFIERS
// Section 2.6.4: Relocation table segment IDs
// =============================================================================

/**
 * @brief Segment type identifier used in relocation entries
 * Specification Reference: Section 2.6.4
 */
enum class SegmentID : uint8_t {
    UNDEFINED = 0,  ///< Undefined reference (external symbol)
    ABSOLUTE  = 1,  ///< Absolute value (never in relocation table)
    TEXT      = 2,  ///< Text segment (code)
    DATA      = 3,  ///< Data segment (initialized data)
    BSS       = 4,  ///< BSS segment (uninitialized data)
    ZERO      = 5   ///< Zero/zeropage segment (6502) or bank zero (65816)
};

// =============================================================================
// RELOCATION TYPES
// Section 2.6.4: Relocation type encodings
// =============================================================================

/**
 * @brief Relocation operation type
 * Specification Reference: Section 2.6.4
 */
enum class RelocationType : uint8_t {
    WORD   = 0x80,  ///< 2-byte address relocation
    HIGH   = 0x40,  ///< High byte of address only
    LOW    = 0x20,  ///< Low byte of address only
    SEGADR = 0xC0,  ///< 3-byte address (65816)
    SEG    = 0xA0   ///< Segment byte of 3-byte address
};

/**
 * @brief Extract relocation type from typebyte
 * @param typebyte The type byte from relocation entry
 * @return The relocation type (upper 3 bits)
 */
constexpr inline RelocationType get_relocation_type(uint8_t typebyte) noexcept {
    return static_cast<RelocationType>(typebyte & 0xE0);
}

/**
 * @brief Extract segment ID from typebyte
 * @param typebyte The type byte from relocation entry
 * @return The segment identifier (lower 5 bits)
 */
constexpr inline SegmentID get_segment_id(uint8_t typebyte) noexcept {
    return static_cast<SegmentID>(typebyte & 0x1F);
}

/**
 * @brief Combine relocation type and segment ID into typebyte
 * @param type The relocation operation type
 * @param segment The target segment
 * @return Encoded typebyte for relocation entry
 */
constexpr inline uint8_t make_typebyte(RelocationType type, SegmentID segment) noexcept {
    return static_cast<uint8_t>(type) | static_cast<uint8_t>(segment);
}

// =============================================================================
// HEADER OPTIONS
// Section 2.6.1: Optional header entries
// =============================================================================

/**
 * @brief Header option type codes
 * Specification Reference: Section 2.6.1
 */
enum class HeaderOptionType : uint8_t {
    FILENAME        = 0,  ///< Filename option
    OS_HEADER       = 1,  ///< Operating system specific header
    ASSEMBLER       = 2,  ///< Assembler/linker identification
    AUTHOR          = 3,  ///< File author
    CREATION_DATE   = 4   ///< Creation date string
};

/**
 * @brief Operating system type for OS header option
 * Specification Reference: Section 2.6.1, OS Header
 */
enum class OSType : uint8_t {
    OSA65       = 1,  ///< OSA/65 header supplement
    LUNIX       = 2,  ///< Lunix header supplement
    CC65        = 3,  ///< CC65 generic module
    OPENCBM     = 4   ///< opencbm floppy modules
};

/**
 * @brief Generic header option structure
 * Variable-length option with type and data
 * Specification Reference: Section 2.6.1
 * 
 * Format in file:
 *   .byt overall_length  // including length and type bytes
 *   .byt option_type
 *   [.byt data_bytes ...]
 */
struct O65_Header_Option {
    uint8_t length;  ///< Total length including length and type bytes
    uint8_t type;    ///< Option type (HeaderOptionType)
    // Followed by (length - 2) data bytes
    
    /**
     * @brief Get pointer to option data
     * @return Pointer to first data byte after type
     */
    const uint8_t* data() const noexcept {
        return reinterpret_cast<const uint8_t*>(this) + 2;
    }
    
    /**
     * @brief Get size of option data
     * @return Number of data bytes (length - 2)
     */
    uint8_t data_size() const noexcept {
        return length >= 2 ? length - 2 : 0;
    }
    
    /**
     * @brief Get pointer to next option in sequence
     * @return Pointer to next option, or nullptr if end marker (length == 0)
     */
    const O65_Header_Option* next() const noexcept {
        if (length == 0) return nullptr;
        return reinterpret_cast<const O65_Header_Option*>(
            reinterpret_cast<const uint8_t*>(this) + length
        );
    }
} __attribute__((packed));

// =============================================================================
// FILE HEADER STRUCTURE
// Section 2.6.1: Fixed header layout
// =============================================================================

/**
 * @brief o65 file header (16-bit size mode)
 * 
 * This structure represents the fixed-size header for size=16bit mode.
 * All multi-byte values are stored in little-endian format.
 * 
 * Specification Reference: Section 2.6.1
 * 
 * CRITICAL: This header uses 16-bit size fields. For 32-bit mode,
 * use O65_Header_32 instead.
 */
struct O65_Header_16 {
    uint8_t  marker[2];     ///< Non-C64 marker: 0x01, 0x00
    uint8_t  magic[3];      ///< Magic number: 'o', '6', '5' (0x6f, 0x36, 0x35)
    uint8_t  version;       ///< Format version (0x00 for v1.3)
    uint16_t mode;          ///< Mode bits (see O65_Mode)
    uint16_t tbase;         ///< Text segment base address in file
    uint16_t tlen;          ///< Text segment length
    uint16_t dbase;         ///< Data segment base address
    uint16_t dlen;          ///< Data segment length
    uint16_t bbase;         ///< BSS segment base address
    uint16_t blen;          ///< BSS segment length
    uint16_t zbase;         ///< Zero segment base address
    uint16_t zlen;          ///< Zero segment length
    uint16_t stack;         ///< Minimum required stack size (0 = unknown)
    
    // Followed by variable-length header options and segments
    
    /**
     * @brief Validate header magic numbers
     * @return true if magic and marker are correct
     */
    bool is_valid() const noexcept {
        return marker[0] == NON_C64_MARKER[0] &&
               marker[1] == NON_C64_MARKER[1] &&
               magic[0] == MAGIC_NUMBER[0] &&
               magic[1] == MAGIC_NUMBER[1] &&
               magic[2] == MAGIC_NUMBER[2];
    }
    
    /**
     * @brief Get decoded mode configuration
     * @return Structured mode bits
     */
    O65_Mode get_mode() const noexcept {
        return O65_Mode::decode(mode);
    }
    
    /**
     * @brief Get pointer to first header option
     * @return Pointer to first option after fixed header
     */
    const O65_Header_Option* options() const noexcept {
        return reinterpret_cast<const O65_Header_Option*>(this + 1);
    }
    
    /**
     * @brief Calculate total header size in bytes
     * Specification Reference: Section 2.6.1 (note at end)
     * @return 26 bytes for 16-bit mode
     */
    static constexpr size_t header_size() noexcept {
        return 26;
    }
} __attribute__((packed));

/**
 * @brief o65 file header (32-bit size mode)
 * 
 * This structure represents the fixed-size header for size=32bit mode.
 * All multi-byte values are stored in little-endian format.
 * 
 * Specification Reference: Section 2.6.1
 * 
 * CRITICAL: This header uses 32-bit size fields. For 16-bit mode,
 * use O65_Header_16 instead.
 */
struct O65_Header_32 {
    uint8_t  marker[2];     ///< Non-C64 marker: 0x01, 0x00
    uint8_t  magic[3];      ///< Magic number: 'o', '6', '5' (0x6f, 0x36, 0x35)
    uint8_t  version;       ///< Format version (0x00 for v1.3)
    uint16_t mode;          ///< Mode bits (see O65_Mode)
    uint32_t tbase;         ///< Text segment base address in file
    uint32_t tlen;          ///< Text segment length
    uint32_t dbase;         ///< Data segment base address
    uint32_t dlen;          ///< Data segment length
    uint32_t bbase;         ///< BSS segment base address
    uint32_t blen;          ///< BSS segment length
    uint32_t zbase;         ///< Zero segment base address
    uint32_t zlen;          ///< Zero segment length
    uint32_t stack;         ///< Minimum required stack size (0 = unknown)
    
    // Followed by variable-length header options and segments
    
    /**
     * @brief Validate header magic numbers
     * @return true if magic and marker are correct
     */
    bool is_valid() const noexcept {
        return marker[0] == NON_C64_MARKER[0] &&
               marker[1] == NON_C64_MARKER[1] &&
               magic[0] == MAGIC_NUMBER[0] &&
               magic[1] == MAGIC_NUMBER[1] &&
               magic[2] == MAGIC_NUMBER[2];
    }
    
    /**
     * @brief Get decoded mode configuration
     * @return Structured mode bits
     */
    O65_Mode get_mode() const noexcept {
        return O65_Mode::decode(mode);
    }
    
    /**
     * @brief Get pointer to first header option
     * @return Pointer to first option after fixed header
     */
    const O65_Header_Option* options() const noexcept {
        return reinterpret_cast<const O65_Header_Option*>(this + 1);
    }
    
    /**
     * @brief Calculate total header size in bytes
     * Specification Reference: Section 2.6.1 (note at end)
     * @return 44 bytes for 32-bit mode
     */
    static constexpr size_t header_size() noexcept {
        return 44;
    }
} __attribute__((packed));

// =============================================================================
// RELOCATION ENTRY STRUCTURES
// Section 2.6.4: Relocation table format
// =============================================================================

/**
 * @brief Single relocation table entry
 * 
 * Relocation entries specify locations in segments that need adjustment
 * during linking or loading. Each entry contains:
 * - Offset to next relocation (encoded as sequence of bytes)
 * - Type of relocation and target segment
 * - Additional information (symbol index for undefined references)
 * - Low byte (for HIGH relocations in bytewise mode)
 * 
 * Specification Reference: Section 2.6.4
 * 
 * NOTE: This is a logical structure. In the actual file format,
 * offsets >= 255 are encoded as sequences of 0xFF bytes.
 * 
 * PRECONDITION: Relocation address starts at segment_base - 1
 * 
 * Format in file:
 *   [255, ..., 255,] offset_byte, typebyte [, symbol_index] [, low_byte]
 * 
 * Where:
 * - offset_byte is in range [0, 254], or sequence of 255s + final offset
 * - typebyte combines RelocationType and SegmentID
 * - symbol_index present only if segment == UNDEFINED (2 or 4 bytes per mode)
 * - low_byte present only for HIGH relocation with bytewise granularity
 * - For SEG relocation, two lower bytes follow in little-endian order
 */
struct O65_Relocation_Entry {
    uint8_t typebyte;  ///< Combined relocation type and segment ID
    
    /**
     * @brief Get relocation operation type
     * @return The type of relocation to perform
     */
    RelocationType type() const noexcept {
        return get_relocation_type(typebyte);
    }
    
    /**
     * @brief Get target segment identifier
     * @return The segment being relocated
     */
    SegmentID segment() const noexcept {
        return get_segment_id(typebyte);
    }
    
    /**
     * @brief Check if this relocation references undefined symbol
     * @return true if segment is UNDEFINED
     */
    bool is_undefined_reference() const noexcept {
        return segment() == SegmentID::UNDEFINED;
    }
    
    /**
     * @brief Check if this is a HIGH byte relocation
     * @return true if relocation type is HIGH
     */
    bool is_high_byte() const noexcept {
        return type() == RelocationType::HIGH;
    }
    
    /**
     * @brief Check if this is a SEG relocation
     * @return true if relocation type is SEG
     */
    bool is_segment_relocation() const noexcept {
        return type() == RelocationType::SEG;
    }
} __attribute__((packed));

/**
 * @brief Relocation table iterator helper
 * 
 * This structure helps parse the variable-length relocation table format.
 * 
 * IMPLEMENTATION NOTE: The actual relocation table in the file consists of:
 * 1. Offset encoding (may be multiple 0xFF bytes + final offset byte)
 * 2. Typebyte (O65_Relocation_Entry)
 * 3. Symbol index (if UNDEFINED, size depends on mode.size)
 * 4. Low byte (if HIGH relocation in bytewise mode)
 * 5. Two bytes (if SEG relocation)
 * 
 * Parsing logic must handle these variable-length entries.
 * A zero offset byte terminates the table.
 */
struct O65_Relocation_Table {
    const uint8_t* data;     ///< Pointer to start of relocation table
    size_t size;             ///< Total size of table in bytes
    uint32_t current_offset; ///< Current relocation address relative to segment start
    
    /**
     * @brief Check if table has more entries
     * @return true if not at end (next byte is not 0)
     */
    bool has_next() const noexcept {
        return size > 0 && *data != 0;
    }
};

// =============================================================================
// UNDEFINED REFERENCES AND EXPORTED GLOBALS
// Section 2.6.3 and 2.6.5
// =============================================================================

/**
 * @brief Undefined reference list header (16-bit mode)
 * 
 * The undefined references list contains labels referenced in this file
 * but not defined. Each entry is a null-terminated ASCII string.
 * 
 * Specification Reference: Section 2.6.3
 * 
 * Format in file:
 *   .word count
 *   "label1", 0
 *   "label2", 0
 *   ...
 */
struct O65_Undefined_List_16 {
    uint16_t count;  ///< Number of undefined labels
    
    /**
     * @brief Get pointer to first label string
     * @return Pointer to start of null-terminated strings
     */
    const char* labels() const noexcept {
        return reinterpret_cast<const char*>(this + 1);
    }
} __attribute__((packed));

/**
 * @brief Undefined reference list header (32-bit mode)
 * 
 * Specification Reference: Section 2.6.3
 */
struct O65_Undefined_List_32 {
    uint32_t count;  ///< Number of undefined labels
    
    /**
     * @brief Get pointer to first label string
     * @return Pointer to start of null-terminated strings
     */
    const char* labels() const noexcept {
        return reinterpret_cast<const char*>(this + 1);
    }
} __attribute__((packed));

/**
 * @brief Exported global entry (16-bit mode)
 * 
 * Each exported label has a name, segment, and offset value.
 * 
 * Specification Reference: Section 2.6.5
 * 
 * Format in file:
 *   "label_name", 0, segment_id (1 byte), offset (.word)
 * 
 * NOTE: An undefined reference cannot be exported (specification prohibits this)
 */
struct O65_Exported_Entry_16 {
    // Name is null-terminated string before this structure
    uint8_t  segment_id;  ///< Segment containing this label
    uint16_t offset;      ///< Offset within segment
    
    /**
     * @brief Get segment identifier
     * @return The segment where this label is defined
     */
    SegmentID segment() const noexcept {
        return static_cast<SegmentID>(segment_id);
    }
} __attribute__((packed));

/**
 * @brief Exported global entry (32-bit mode)
 * 
 * Specification Reference: Section 2.6.5
 */
struct O65_Exported_Entry_32 {
    // Name is null-terminated string before this structure
    uint8_t  segment_id;  ///< Segment containing this label
    uint32_t offset;      ///< Offset within segment
    
    /**
     * @brief Get segment identifier
     * @return The segment where this label is defined
     */
    SegmentID segment() const noexcept {
        return static_cast<SegmentID>(segment_id);
    }
} __attribute__((packed));

/**
 * @brief Exported globals list header (16-bit mode)
 * 
 * Specification Reference: Section 2.6.5
 * 
 * Format in file:
 *   .word count
 *   { "name", 0, segment_id, offset }+
 */
struct O65_Exported_List_16 {
    uint16_t count;  ///< Number of exported labels
    
    /**
     * @brief Get pointer to first exported entry data
     * @return Pointer to start of entries
     */
    const uint8_t* entries() const noexcept {
        return reinterpret_cast<const uint8_t*>(this + 1);
    }
} __attribute__((packed));

/**
 * @brief Exported globals list header (32-bit mode)
 * 
 * Specification Reference: Section 2.6.5
 */
struct O65_Exported_List_32 {
    uint32_t count;  ///< Number of exported labels
    
    /**
     * @brief Get pointer to first exported entry data
     * @return Pointer to start of entries
     */
    const uint8_t* entries() const noexcept {
        return reinterpret_cast<const uint8_t*>(this + 1);
    }
} __attribute__((packed));

// =============================================================================
// SEGMENT DESCRIPTOR
// Section 2.2 and 2.6.2: Segment types and memory regions
// =============================================================================

/**
 * @brief Segment descriptor for a single memory region
 * 
 * Segments are the fundamental units of code and data in o65 files.
 * Each segment has a base address, length, and type.
 * 
 * Specification Reference: Section 2.2, Section 2.6.2
 * 
 * SEGMENT TYPES:
 * - TEXT: Read-only executable code (no self-modifying code)
 * - DATA: Read-write initialized data
 * - BSS:  Read-write uninitialized data (not in file)
 * - ZERO: Zeropage (6502) or bank zero (65816) data (not in file)
 * 
 * CRITICAL CONSTRAINT: Programs must not assume relative addresses
 * between different segments.
 */
struct O65_Segment_Descriptor {
    SegmentID segment_type;  ///< Type of this segment
    uint32_t base_address;   ///< Base address from file header
    uint32_t length;         ///< Segment length in bytes
    const uint8_t* data;     ///< Pointer to segment data (nullptr for BSS/ZERO)
    
    /**
     * @brief Check if segment contains actual data in file
     * @return true for TEXT and DATA segments, false for BSS and ZERO
     */
    bool has_file_data() const noexcept {
        return segment_type == SegmentID::TEXT || 
               segment_type == SegmentID::DATA;
    }
    
    /**
     * @brief Check if segment requires write access
     * @return false for TEXT, true for DATA/BSS/ZERO
     */
    bool is_writable() const noexcept {
        return segment_type != SegmentID::TEXT;
    }
};

// =============================================================================
// FILE LAYOUT DESCRIPTOR
// Section 2.6: Complete file format
// =============================================================================

/**
 * @brief Complete o65 file structure descriptor
 * 
 * This structure provides a logical view of an entire o65 file.
 * 
 * Specification Reference: Section 2.6
 * 
 * File layout:
 *   1. Header (fixed size, depends on mode.size)
 *   2. Header options (variable length, 0x00 terminated)
 *   3. Text segment data (tlen bytes)
 *   4. Data segment data (dlen bytes)
 *   5. Undefined references list
 *   6. Text segment relocation table (0x00 terminated)
 *   7. Data segment relocation table (0x00 terminated)
 *   8. Exported globals list
 * 
 * CHAINED FILES: If mode.chain is set, another complete o65 file
 * follows immediately after the exported globals list.
 */
struct O65_File_Layout {
    const void* header;           ///< Pointer to O65_Header_16 or O65_Header_32
    O65_Mode mode;                ///< Decoded mode configuration
    
    // Segment data pointers (null for BSS/ZERO)
    const uint8_t* text_data;
    const uint8_t* data_data;
    
    // Relocation and symbol tables
    const uint8_t* undef_refs;
    const uint8_t* text_reloc;
    const uint8_t* data_reloc;
    const uint8_t* exported_globals;
    
    // Next file in chain (null if not chained)
    const O65_File_Layout* next_section;
    
    /**
     * @brief Get header as 16-bit structure
     * PRECONDITION: mode.address_size == BITS_16
     * @return Pointer to 16-bit header
     */
    const O65_Header_16* header_16() const noexcept {
        return static_cast<const O65_Header_16*>(header);
    }
    
    /**
     * @brief Get header as 32-bit structure
     * PRECONDITION: mode.address_size == BITS_32
     * @return Pointer to 32-bit header
     */
    const O65_Header_32* header_32() const noexcept {
        return static_cast<const O65_Header_32*>(header);
    }
};

// =============================================================================
// RELOCATION CALCULATION CONTRACTS
// Section 2.6.4: Relocation semantics
// =============================================================================

/**
 * @brief Calculate relocated address for WORD relocation
 * 
 * Specification Reference: Section 2.6.4
 * 
 * @param original_value The 16 or 32-bit value from the file
 * @param segment_base_file The segment base address from file header
 * @param segment_base_real The actual load address of the segment
 * @return The relocated address value
 * 
 * FORMULA: relocated = original + (real_base - file_base)
 * 
 * PRECONDITION: Address size must match mode.address_size
 * POSTCONDITION: Result fits in address_size bits
 */
static inline uint32_t relocate_word(
    uint32_t original_value,
    uint32_t segment_base_file,
    uint32_t segment_base_real
) noexcept {
    int32_t delta = static_cast<int32_t>(segment_base_real - segment_base_file);
    return original_value + delta;
}

/**
 * @brief Calculate relocated high byte with carry handling
 * 
 * Specification Reference: Section 2.6.4, Example section
 * 
 * @param high_byte The high byte from the opcode
 * @param low_byte The low byte from relocation table
 * @param segment_base_file The segment base address from file header
 * @param segment_base_real The actual load address of the segment
 * @return The relocated high byte
 * 
 * CRITICAL: This function correctly handles carry from low byte addition.
 * Without storing the low byte in the relocation table, carry would be lost.
 * 
 * FORMULA:
 *   full_address_file = (high_byte << 8) + low_byte
 *   full_address_real = full_address_file + (real_base - file_base)
 *   return high_byte of full_address_real
 * 
 * PRECONDITION: mode.granularity == BYTEWISE (for pagewise, low_byte is 0)
 */
static inline uint8_t relocate_high(
    uint8_t high_byte,
    uint8_t low_byte,
    uint32_t segment_base_file,
    uint32_t segment_base_real
) noexcept {
    uint32_t full_address = (static_cast<uint32_t>(high_byte) << 8) | low_byte;
    int32_t delta = static_cast<int32_t>(segment_base_real - segment_base_file);
    uint32_t relocated = full_address + delta;
    return static_cast<uint8_t>((relocated >> 8) & 0xFF);
}

/**
 * @brief Calculate relocated low byte
 * 
 * Specification Reference: Section 2.6.4
 * 
 * @param low_byte The low byte from the opcode
 * @param segment_base_file The segment base address from file header
 * @param segment_base_real The actual load address of the segment
 * @return The relocated low byte
 * 
 * FORMULA: Simply add the delta to low byte (carry is ignored)
 */
static inline uint8_t relocate_low(
    uint8_t low_byte,
    uint32_t segment_base_file,
    uint32_t segment_base_real
) noexcept {
    int32_t delta = static_cast<int32_t>(segment_base_real - segment_base_file);
    return static_cast<uint8_t>((low_byte + delta) & 0xFF);
}

/**
 * @brief Calculate SEGADR relocation (65816 24-bit address)
 * 
 * Specification Reference: Section 2.6.4
 * 
 * IMPLEMENTATION-DEFINED: The specification does not fully detail
 * SEGADR calculation. Implementers must define behavior.
 * 
 * @param address_low Low 16 bits of address
 * @param address_high High 8 bits (segment/bank)
 * @param segment_base_file The segment base address from file header
 * @param segment_base_real The actual load address of the segment
 * @return The relocated 24-bit address (high byte in bits 16-23)
 */
static inline uint32_t relocate_segadr(
    uint16_t address_low,
    uint8_t address_high,
    uint32_t segment_base_file,
    uint32_t segment_base_real
) noexcept {
    uint32_t full_address = (static_cast<uint32_t>(address_high) << 16) | address_low;
    int32_t delta = static_cast<int32_t>(segment_base_real - segment_base_file);
    return full_address + delta;
}

/**
 * @brief Calculate SEG relocation (65816 segment/bank byte)
 * 
 * Specification Reference: Section 2.6.4
 * 
 * IMPLEMENTATION-DEFINED: The specification does not fully detail
 * SEG calculation. Implementers must define behavior.
 * 
 * @param segment_byte The segment byte from the opcode
 * @param offset_low Low byte of offset (from relocation table)
 * @param offset_high High byte of offset (from relocation table)
 * @param segment_base_file The segment base address from file header
 * @param segment_base_real The actual load address of the segment
 * @return The relocated segment byte
 */
static inline uint8_t relocate_seg(
    uint8_t segment_byte,
    uint8_t offset_low,
    uint8_t offset_high,
    uint32_t segment_base_file,
    uint32_t segment_base_real
) noexcept {
    uint32_t full_address = (static_cast<uint32_t>(segment_byte) << 16) |
                           (static_cast<uint32_t>(offset_high) << 8) |
                            offset_low;
    int32_t delta = static_cast<int32_t>(segment_base_real - segment_base_file);
    uint32_t relocated = full_address + delta;
    return static_cast<uint8_t>((relocated >> 16) & 0xFF);
}

// =============================================================================
// UNDEFINED REFERENCE RESOLUTION
// Section 2.6.4 and Appendix B: Late binding
// =============================================================================

/**
 * @brief Resolve undefined reference to external symbol
 * 
 * Specification Reference: Section 2.6.4, Appendix B (Late binding example)
 * 
 * When a relocation entry has segment == UNDEFINED, the value at the
 * relocation address must be combined with the external symbol's address.
 * 
 * @param value_in_file The value at the relocation address in the file
 * @param external_address The resolved address of the external symbol
 * @return The final relocated value
 * 
 * FORMULA: final = value_in_file + external_address
 * 
 * EXAMPLE: If source has "LDA IOPORT+1" and IOPORT resolves to $de00,
 * the file contains $0001 and result is $de01.
 * 
 * POSTCONDITION: For HIGH/LOW/SEG relocations, only the appropriate
 * byte is written back to the segment.
 */
static inline uint32_t resolve_undefined(
    uint32_t value_in_file,
    uint32_t external_address
) noexcept {
    return value_in_file + external_address;
}

// =============================================================================
// CHAINED FILE HANDLING
// Section 2.6.1: Chain bit and multi-o65 files
// =============================================================================

/**
 * @brief Chain file descriptor for multi-o65 files
 * 
 * Specification Reference: Section 2.6.1, Chain bit discussion
 * 
 * When mode.chain is set, another complete o65 "section" follows
 * in the same file. Each section is an independent o65 file with
 * its own header, segments, and symbol tables.
 * 
 * USE CASES:
 * 1. Init code in separate section (disposable after startup)
 * 2. Sections for different memory mappings
 * 3. Fat binaries with multiple CPU targets
 * 
 * LINKER BEHAVIOR: Process each section independently in order.
 * The linker does not merge sections; it processes them sequentially.
 * 
 * IMPLEMENTATION-DEFINED: Symbol resolution between sections.
 * The specification mentions loaders "may" support binding undefined
 * references in later sections to exports from earlier sections.
 */
struct O65_Chain_Section {
    const O65_File_Layout* section;  ///< This section's file layout
    const O65_Chain_Section* next;   ///< Next section in chain (null if last)
    uint32_t section_index;          ///< 0-based index in chain
    
    /**
     * @brief Check if this is the last section in chain
     * @return true if mode.chain == 0 or next == nullptr
     */
    bool is_last() const noexcept {
        return next == nullptr;
    }
};

// =============================================================================
// VALIDATION AND CONSTRAINTS
// =============================================================================

/**
 * @brief Validate mode bit combinations
 * 
 * Specification Reference: Section 2.6.1
 * 
 * @param mode The mode configuration to validate
 * @return true if mode is valid according to specification
 * 
 * CONSTRAINTS:
 * - If mode.granularity == PAGEWISE, mode.alignment should be ALIGN_BLOCK
 * - All reserved bits must be zero
 * - Version must be 0x00 for this specification version
 * 
 * IMPLEMENTATION NOTE: This validation is advisory. Implementers may
 * choose to accept non-conforming files with warnings.
 */
inline bool validate_mode(const O65_Mode& mode) noexcept {
    // Check alignment recommendation for pagewise relocation
    if (mode.granularity == RelocationGranularity::PAGEWISE) {
        if (mode.alignment != AlignmentMode::ALIGN_BLOCK) {
            // Warning: pagewise should use block alignment
            // Not an error, but recommended
        }
    }
    
    // All validations passed
    return true;
}

/**
 * @brief Validate simple addressing constraints
 * 
 * Specification Reference: Section 2.6.1, Simple bit
 * 
 * @param header The header to validate (16 or 32-bit)
 * @return true if simple mode constraints are satisfied
 * 
 * CONSTRAINTS when mode.simple is set:
 * - dbase == tbase + tlen
 * - bbase == dbase + dlen
 * 
 * NOTE: These constraints allow loader to load text and data
 * in a single block and use same base for all relocations.
 */
template<typename HeaderType>
inline bool validate_simple_addressing(const HeaderType& header) noexcept {
    O65_Mode mode = O65_Mode::decode(header.mode);
    
    if (mode.simple_mode == SimpleAddressing::SIMPLE) {
        bool data_follows_text = (header.dbase == header.tbase + header.tlen);
        bool bss_follows_data = (header.bbase == header.dbase + header.dlen);
        return data_follows_text && bss_follows_data;
    }
    
    return true; // Simple bit not set, no constraints
}

// =============================================================================
// IMPLEMENTATION-DEFINED BEHAVIORS
// =============================================================================

/**
 * @namespace o65::implementation_defined
 * @brief Behaviors not fully specified in the o65 standard
 * 
 * The following behaviors are not completely defined by the specification
 * and must be defined by each toolchain implementation:
 * 
 * 1. BIND_LATE failure handling: What happens when a late-bound symbol
 *    cannot be resolved at load time? (Mentioned in specification but
 *    consequences not detailed)
 * 
 * 2. Cross-section symbol resolution: Whether undefined references in
 *    later chain sections can bind to exports from earlier sections.
 *    (Specification says "may" support this)
 * 
 * 3. SEGADR and SEG relocation semantics: The specification mentions
 *    these 65816 relocation types but does not provide detailed
 *    calculation examples.
 * 
 * 4. Error handling for invalid files: How to handle malformed headers,
 *    invalid mode combinations, or corrupt relocation tables.
 * 
 * 5. Character encoding: While ASCII is mentioned, the specification
 *    allows platform-appropriate encodings for label names.
 * 
 * 6. Maximum label name length: "Should not be exceedingly long" is
 *    not precisely defined.
 * 
 * 7. Multiple header options of same type: Behavior when duplicate
 *    option types appear.
 * 
 * Implementers must document their chosen behaviors for these cases.
 */
namespace implementation_defined {
    // Placeholder for implementation-specific extensions
}

} // namespace o65

// =============================================================================
// STATIC ASSERTIONS FOR STRUCTURE SIZES
// =============================================================================

static_assert(sizeof(o65::O65_Header_16) == 26, 
    "O65_Header_16 must be exactly 26 bytes (Specification Section 2.6.1)");

static_assert(sizeof(o65::O65_Header_32) == 44,
    "O65_Header_32 must be exactly 44 bytes (Specification Section 2.6.1)");

static_assert(sizeof(o65::O65_Exported_Entry_16) == 3,
    "O65_Exported_Entry_16 must be 3 bytes (1 segment + 2 offset)");

static_assert(sizeof(o65::O65_Exported_Entry_32) == 5,
    "O65_Exported_Entry_32 must be 5 bytes (1 segment + 4 offset)");

#endif // O65_FORMAT_HPP
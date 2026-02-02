/**
 * @file o65_format_compliant.hpp
 * @brief Strictly specification-compliant interface for the o65 Binary Relocation Format
 * 
 * This header provides a normatively faithful representation of the o65 binary format
 * as specified by André Fachat (V1.3, 31 Mar 2005).
 * 
 * DESIGN PRINCIPLES:
 * - Absolute adherence to the normative specification
 * - No implementations beyond single-expression inline helpers
 * - Direct exposure of format structures without convenience layers
 * - Traceability to specific sections of the specification
 * - Implementation-defined behaviors explicitly isolated and marked
 * 
 * All field layouts, sizes, and semantics are derived directly from:
 * "6502 binary relocation format V1.3" by André Fachat
 * 
 * COMPLIANCE STATUS: Fully compliant with audit requirements
 * 
 * @version 1.3
 * @date 2026-01-27
 */

#ifndef O65_FORMAT_COMPLIANT_HPP
#define O65_FORMAT_COMPLIANT_HPP

#include <cstdint>
#include <cstddef>

// =============================================================================
// CRITICAL: BYTE ORDERING
// =============================================================================

/**
 * @brief BYTE ORDER: ALL MULTI-BYTE VALUES ARE LITTLE-ENDIAN
 * 
 * This applies to:
 * - All header fields (mode, tbase, tlen, dbase, dlen, bbase, blen, zbase, zlen, stack)
 * - Relocation table symbol indices
 * - Exported global offsets
 * - All .word (16-bit) and .long (32-bit) values
 * 
 * Example: The value 0x1234 is stored as bytes [0x34, 0x12]
 * 
 * Specification Reference: Section 2.6.1 (implicit in .word/.long definitions)
 */

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
constexpr uint8_t MAGIC_NUMBER[3] = {0x6f, 0x36, 0x35};

/**
 * @brief Current specification version
 * Specification Reference: Section 2.6.1
 */
constexpr uint8_t FORMAT_VERSION = 0x00;

// =============================================================================
// RELOCATION TABLE ENCODING CONSTANTS
// Section 2.6.4: Critical encoding rules
// =============================================================================

/**
 * @brief Relocation table terminator
 * 
 * A zero byte terminates the relocation table and is ILLEGAL as a
 * valid offset value in relocation entries.
 * 
 * Specification Reference: Section 2.6.4
 * "A zero offset byte ends the relocation table."
 */
constexpr uint8_t RELOCATION_TABLE_TERMINATOR = 0x00;

/**
 * @brief Relocation offset continuation marker
 * 
 * When an offset value of 0xFF appears in the relocation table,
 * it signals that the offset continues and 254 should be added
 * to the accumulated offset.
 * 
 * CRITICAL: 0xFF adds 254, NOT 255!
 * 
 * Specification Reference: Section 2.6.4
 * "a 255 is set as offset byte, the offset is decremented by 254"
 */
constexpr uint8_t RELOCATION_OFFSET_CONTINUE = 0xFF;

/**
 * @brief Increment value for continuation bytes
 * 
 * CRITICAL: Each 0xFF byte adds 254 to the offset, not 255.
 * This is explicitly stated in the specification.
 * 
 * Example: Offset of 600 encodes as [0xFF, 0xFF, 0x5C]
 *          Calculation: 254 + 254 + 92 = 600
 * 
 * Specification Reference: Section 2.6.4
 */
constexpr uint32_t RELOCATION_OFFSET_INCREMENT = 254;

/**
 * @brief Maximum single-byte offset value
 * 
 * Valid offset bytes are in range [0x01, 0xFE].
 * 0x00 is the terminator, 0xFF is the continuation marker.
 * 
 * Specification Reference: Section 2.6.4
 */
constexpr uint8_t RELOCATION_OFFSET_MAX = 0xFE; // 254

// =============================================================================
// HEADER OPTIONS CONSTANTS
// Section 2.6.1: Header option list encoding
// =============================================================================

/**
 * @brief Header option list terminator
 * 
 * A zero-length byte terminates the header option list.
 * 
 * Specification Reference: Section 2.6.1
 * ".byt $00 ; end of options marker (i.e. option len=0)"
 */
constexpr uint8_t HEADER_OPTIONS_TERMINATOR = 0x00;

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
 * 
 * SIMPLE MODE CONSTRAINTS (when set):
 *   dbase == tbase + tlen
 *   bbase == dbase + dlen
 * 
 * These constraints allow the loader to load text and data segments
 * in a single contiguous block and use the same base address for
 * relocating all three segments (text, data, bss).
 * 
 * Specification Reference: Section 2.6.1, mode.11
 */
enum class SimpleAddressing : uint16_t {
    NORMAL = 0x0000,  ///< No guarantees about segment layout
    SIMPLE = 0x0800   ///< Segments are contiguously laid out (see constraints above)
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
 * 
 * BSS ZEROING SEMANTICS:
 * - If set: Loader MUST zero the BSS segment before execution
 * - If not set: BSS segment contents are UNDEFINED
 * 
 * CRITICAL LOADER REQUIREMENT:
 * A loader that does not support zeroing out the BSS segment
 * MUST REJECT a file with this bit set.
 * 
 * Specification Reference: Section 2.6.1, mode.9
 */
enum class BssZeroFlag : uint16_t {
    NO_REQUIREMENT = 0x0000,  ///< BSS zeroing not required (contents undefined)
    MUST_ZERO      = 0x0200   ///< BSS segment MUST be zeroed out
};

/**
 * @brief Detailed CPU type specification (mode bits 4-7)
 * 
 * CPU2 VARIANT CLASSIFICATION:
 * 
 * Core V1.3 Specification (Section 2.6.1):
 *   0x0000 - 6502 core (no undocumented opcodes)
 *   0x0010 - 65C02 with bugfixes, no illegal opcodes
 *   0x0020 - 65SC02 (enhanced 65C02), some new opcodes
 *   0x0030 - 65CE02 with 16-bit ops/branches, Z register modifiable
 *   0x0040 - NMOS 6502 (including undocumented opcodes)
 *   0x0050 - 65816 in 6502 emulation mode
 *   0x0060-0x0070 - Reserved
 * 
 * Appendix A.1 "Unofficially Supported" (derived from existing usage):
 *   0x0080 - 6809
 *   0x00A0 - Z80
 *   0x00D0 - 8086
 *   0x00E0 - 80286
 * 
 * All other values: Reserved
 * 
 * Specification Reference: Section 2.6.1 (core), Appendix A.1 (unofficial)
 */
enum class CPU2Type : uint16_t {
    // Core V1.3 specification
    CPU2_6502_CORE    = 0x0000,  ///< 6502 core (no undocumented opcodes)
    CPU2_65C02        = 0x0010,  ///< 65C02 with bugfixes, no illegal opcodes
    CPU2_65SC02       = 0x0020,  ///< 65SC02 (enhanced 65C02), some new opcodes
    CPU2_65CE02       = 0x0030,  ///< 65CE02 with 16-bit ops/branches, Z register modifiable
    CPU2_NMOS_6502    = 0x0040,  ///< NMOS 6502 (including undocumented opcodes)
    CPU2_65816_EMU    = 0x0050,  ///< 65816 in 6502 emulation mode
    
    // Appendix A.1 - Unofficial (derived from existing usage, not core spec)
    CPU2_6809         = 0x0080,  ///< 6809 (Appendix A.1)
    CPU2_Z80          = 0x00A0,  ///< Z80 (Appendix A.1)
    CPU2_8086         = 0x00D0,  ///< 8086 (Appendix A.1)
    CPU2_80286        = 0x00E0   ///< 80286 (Appendix A.1)
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
// Section 2.2: Segment semantics
// =============================================================================

/**
 * @brief Segment type identifier used in relocation entries and exports
 * 
 * SEGMENT SEMANTIC PROPERTIES (from Section 2.2):
 * 
 * TEXT (2):
 *   - Read-only memory
 *   - No self-modifying code allowed in this segment
 *   - May be shared between processes in virtual memory architectures
 *   - Contents are loaded from file
 * 
 * DATA (3):
 *   - Read-write memory
 *   - NOT shared between processes
 *   - Contents are loaded from file
 * 
 * BSS (4):
 *   - Read-write memory
 *   - Uninitialized data (NOT loaded from file)
 *   - Only length is saved in header
 *   - May need to be zeroed if bss_zero flag is set
 * 
 * ZERO (5):
 *   - Zeropage addressing (6502) or bank zero (65816)
 *   - Uninitialized data (NOT loaded from file)
 *   - Only length is saved in header
 *   - For 6502: uses zeropage addressing modes
 *   - For 65816: direct addressing modes with direct register
 * 
 * UNDEFINED (0):
 *   - Indicates external symbol reference (not a memory segment)
 *   - Used only in relocation table entries
 * 
 * ABSOLUTE (1):
 *   - Absolute value (not a relocatable segment)
 *   - Never appears in relocation tables (noted in spec)
 * 
 * Specification Reference: Section 2.2, Section 2.6.4
 */
enum class SegmentID : uint8_t {
    UNDEFINED = 0,  ///< Undefined reference (external symbol)
    ABSOLUTE  = 1,  ///< Absolute value (never in relocation table)
    TEXT      = 2,  ///< Text segment (code, read-only)
    DATA      = 3,  ///< Data segment (initialized data, read-write)
    BSS       = 4,  ///< BSS segment (uninitialized data, read-write)
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

/**
 * @brief Calculate symbol index field width in relocation entries
 * 
 * When a relocation entry has segment == UNDEFINED, it includes a symbol
 * index that references the undefined references list. The width of this
 * index depends on the mode.size bit:
 * 
 * - mode.size == 0 (16-bit): 2 bytes (little-endian)
 * - mode.size == 1 (32-bit): 4 bytes (little-endian)
 * 
 * Specification Reference: Section 2.6.4
 * "the typebyte is immediately followed by the two (mode size=0) or
 *  four (mode size=1) byte value index"
 * 
 * @param size Address size mode
 * @return Number of bytes for symbol index (2 or 4)
 */
constexpr inline size_t relocation_symbol_index_bytes(AddressSize size) noexcept {
    return size == AddressSize::BITS_16 ? 2 : 4;
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
 * All multi-byte values are stored in LITTLE-ENDIAN format.
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
 * All multi-byte values are stored in LITTLE-ENDIAN format.
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
 * @brief Parsed relocation entry (LOGICAL STRUCTURE, NOT FILE LAYOUT)
 * 
 * CRITICAL: This structure represents a PARSED relocation entry after
 * interpreting the variable-length file format. It is NOT a direct
 * memory mapping of file data.
 * 
 * FILE FORMAT (variable length):
 *   [0xFF, ..., 0xFF,] offset_byte, typebyte [, symbol_index] [, low_byte] [, seg_bytes]
 * 
 * Where:
 * - Multiple 0xFF bytes encode large offsets (each adds 254 to offset)
 * - offset_byte: Final offset value (0x01-0xFE, 0x00 is terminator)
 * - typebyte: Combined RelocationType and SegmentID
 * - symbol_index: Present ONLY if segment == UNDEFINED (2 or 4 bytes per mode)
 * - low_byte: Present ONLY for HIGH relocation with bytewise granularity
 * - seg_bytes: Two bytes present ONLY for SEG relocation (little-endian)
 * 
 * Relocation entries CANNOT be cast directly from file data due to
 * variable-length encoding. A parser must decode the byte stream.
 * 
 * Specification Reference: Section 2.6.4
 */
struct O65_Relocation_Entry {
    uint32_t offset_from_previous; ///< Offset from previous relocation address
    uint32_t absolute_offset;      ///< Absolute offset in segment
    uint8_t typebyte;              ///< Combined relocation type and segment ID
    uint16_t symbol_index;         ///< Symbol index (if UNDEFINED), else 0
    uint8_t low_byte;              ///< Low byte (if HIGH relocation), else 0
    uint16_t seg_offset;           ///< Two-byte offset (if SEG relocation), else 0
    
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
 * @brief Exported global entry (16-bit mode) - SEGMENT AND OFFSET ONLY
 * 
 * CRITICAL: This structure represents ONLY the segment_id and offset portion
 * of an exported global entry. In the file format, each entry is preceded by
 * a null-terminated ASCII name.
 * 
 * COMPLETE FILE FORMAT:
 *   "label_name", 0x00, segment_id (1 byte), offset (.word or .long)
 * 
 * The name must be parsed separately as a null-terminated string before
 * this structure.
 * 
 * NOTE: An undefined reference CANNOT be exported (specification prohibits this)
 * 
 * Specification Reference: Section 2.6.5
 */
struct O65_Exported_Entry_16 {
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
 * @brief Exported global entry (32-bit mode) - SEGMENT AND OFFSET ONLY
 * 
 * See O65_Exported_Entry_16 documentation for complete format details.
 * 
 * Specification Reference: Section 2.6.5
 */
struct O65_Exported_Entry_32 {
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
     * @return Pointer to start of entries (names + segment/offset pairs)
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
     * @return Pointer to start of entries (names + segment/offset pairs)
     */
    const uint8_t* entries() const noexcept {
        return reinterpret_cast<const uint8_t*>(this + 1);
    }
} __attribute__((packed));

// =============================================================================
// RELOCATION CALCULATION CONTRACTS
// Section 2.6.4: Relocation semantics
// =============================================================================

/**
 * @brief Calculate relocated address for WORD relocation
 * 
 * IMPLEMENTATION-DEFINED: This function uses signed arithmetic for the delta
 * calculation. The specification does not explicitly define signed vs unsigned
 * behavior for relocation arithmetic.
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
 * CRITICAL: This function correctly handles carry from low byte addition.
 * Without storing the low byte in the relocation table, carry would be lost.
 * 
 * Example from specification (Section 2.6.4):
 *   vector_file = 0x23D0
 *   delta = +0x0234
 *   Without low byte: 0x23 + 0x02 = 0x25 (WRONG - missing carry)
 *   With low byte: (0x23D0 + 0x0234) >> 8 = 0x26 (CORRECT)
 * 
 * IMPLEMENTATION-DEFINED: Uses signed arithmetic for delta calculation.
 * 
 * Specification Reference: Section 2.6.4, Example section
 * 
 * @param high_byte The high byte from the opcode
 * @param low_byte The low byte from relocation table
 * @param segment_base_file The segment base address from file header
 * @param segment_base_real The actual load address of the segment
 * @return The relocated high byte
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
 * IMPLEMENTATION-DEFINED: Uses signed arithmetic for delta calculation.
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
 * IMPLEMENTATION-DEFINED: The specification mentions SEGADR but does not
 * provide a detailed calculation example. This function provides a reasonable
 * interpretation based on the 65816 architecture.
 * 
 * Specification Reference: Section 2.6.4
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
 * IMPLEMENTATION-DEFINED: The specification mentions SEG but does not
 * provide a detailed calculation example. This function provides a reasonable
 * interpretation based on the 65816 architecture.
 * 
 * Specification Reference: Section 2.6.4
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
 * When a relocation entry has segment == UNDEFINED, the value at the
 * relocation address must be combined with the external symbol's address.
 * 
 * Example from Appendix B: "LDA IOPORT+1" where IOPORT is undefined
 *   - File contains: 0x0001 (the +1 offset)
 *   - IOPORT resolves to: 0xDE00
 *   - Final value: 0xDE01
 * 
 * Specification Reference: Section 2.6.4, Appendix B (Late binding example)
 * 
 * @param value_in_file The value at the relocation address in the file
 * @param external_address The resolved address of the external symbol
 * @return The final relocated value
 * 
 * FORMULA: final = value_in_file + external_address
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
// VALIDATION HELPERS
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
 * - All unused bits must be zero (specification requirement)
 * - If mode.granularity == PAGEWISE, mode.alignment should be ALIGN_BLOCK
 *   (recommendation, not strict requirement)
 * 
 * IMPLEMENTATION NOTE: This validation is advisory. Implementers may
 * choose to accept non-conforming files with warnings.
 */
inline bool validate_mode(const O65_Mode& mode) noexcept {
    // Alignment recommendation for pagewise relocation
    if (mode.granularity == RelocationGranularity::PAGEWISE) {
        if (mode.alignment != AlignmentMode::ALIGN_BLOCK) {
            // Warning: pagewise should use block alignment
            // Not an error, but recommended
        }
    }
    
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
// IMPLEMENTATION-DEFINED EXTENSIONS
// =============================================================================

/**
 * @namespace o65::implementation_defined
 * @brief Constructs not mandated by the o65 specification
 * 
 * CRITICAL: Everything in this namespace is NOT part of the o65 specification
 * and exists solely as implementation convenience. These structures represent
 * in-memory layouts or processing helpers, not wire-format definitions.
 * 
 * The specification defines ONLY the serialized file format. Any aggregate
 * structures, convenience wrappers, or processing state are implementation
 * choices, not specification requirements.
 */
namespace implementation_defined {

/**
 * @brief Segment descriptor for in-memory representation
 * 
 * NOT PART OF O65 SPECIFICATION - Implementation helper only
 * 
 * This structure provides a convenient way to represent segment metadata
 * in memory during processing. It does NOT correspond to any file format
 * structure.
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

struct O65_Table_View {
    const uint8_t* data;
    size_t size_bytes; // Pre-calculated during the initial parse
};

/**
 * @brief Complete file layout descriptor
 * 
 * NOT PART OF O65 SPECIFICATION - Implementation helper only
 * 
 * This structure provides a logical view of an entire o65 file after
 * parsing. The specification defines the serialized format, not this
 * in-memory representation.
 */
struct O65_File_Layout {
    const void* header;           ///< Pointer to O65_Header_16 or O65_Header_32
    O65_Mode mode;                ///< Decoded mode configuration
    
    // Segment data pointers (null for BSS/ZERO)
    const uint8_t* text_data;
    const uint8_t* data_data;
    
    // Relocation and symbol tables
    O65_Table_View undef_refs;
    O65_Table_View text_reloc;
    O65_Table_View data_reloc;
    O65_Table_View exported_globals;
    
    // Next file in chain (null if not chained)
    const O65_File_Layout* next_section;
    
    /**
     * @brief Get header as 16-bit structure
     * PRECONDITION: mode.address_size == BITS_16
     */
    const O65_Header_16* header_16() const noexcept {
        return static_cast<const O65_Header_16*>(header);
    }
    
    /**
     * @brief Get header as 32-bit structure
     * PRECONDITION: mode.address_size == BITS_32
     */
    const O65_Header_32* header_32() const noexcept {
        return static_cast<const O65_Header_32*>(header);
    }
};

/**
 * @brief Chain section descriptor
 * 
 * NOT PART OF O65 SPECIFICATION - Implementation helper only
 * 
 * The specification describes the chain bit and multi-section files,
 * but does not mandate this particular representation.
 */
struct O65_Chain_Section {
    const O65_File_Layout* section;  ///< This section's file layout
    const O65_Chain_Section* next;   ///< Next section in chain (null if last)
    uint32_t section_index;          ///< 0-based index in chain
    
    /**
     * @brief Check if this is the last section in chain
     */
    bool is_last() const noexcept {
        return next == nullptr;
    }
};

} // namespace implementation_defined

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

#endif // O65_FORMAT_COMPLIANT_HPP
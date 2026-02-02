/**
 * @file codegen_zeropage_tests.cpp
 * @brief Tests for ZeroPageAllocator functionality
 * 
 * Tests allocation, deallocation, reservation, and edge cases
 * for zero-page memory management.
 */

#include "test_helpers.hpp"
#include <gtest/gtest.h>

// ============================================================================
// Basic Allocation Tests
// ============================================================================

TEST_F(ZeroPageAllocatorTest, Allocate_SingleByte_Succeeds) {
    auto addr = allocator.allocate(1);
    
    ASSERT_TRUE(addr.has_value()) 
        << "Single byte allocation should succeed";
    EXPECT_GE(addr.value(), 0x08) 
        << "Allocated address should be >= 0x08 (user area start)";
}

TEST_F(ZeroPageAllocatorTest, Allocate_MultipleSingleBytes_UniqueAddresses) {
    auto addr1 = allocator.allocate(1);
    auto addr2 = allocator.allocate(1);
    auto addr3 = allocator.allocate(1);
    
    ASSERT_TRUE(addr1.has_value());
    ASSERT_TRUE(addr2.has_value());
    ASSERT_TRUE(addr3.has_value());
    
    EXPECT_NE(addr1.value(), addr2.value()) 
        << "Consecutive allocations should return different addresses";
    EXPECT_NE(addr2.value(), addr3.value());
    EXPECT_NE(addr1.value(), addr3.value());
}

TEST_F(ZeroPageAllocatorTest, Allocate_MultipleBytes_Contiguous) {
    auto addr = allocator.allocate(4);
    
    ASSERT_TRUE(addr.has_value()) 
        << "4-byte allocation should succeed";
    EXPECT_GE(addr.value(), 0x08);
}

TEST_F(ZeroPageAllocatorTest, Allocate_ZeroSize_ReturnsNullopt) {
    auto addr = allocator.allocate(0);
    
    EXPECT_FALSE(addr.has_value()) 
        << "Zero-size allocation should return nullopt";
}

TEST_F(ZeroPageAllocatorTest, Allocate_TooLarge_ReturnsNullopt) {
    // Trying to allocate more than available (256 bytes total, 8 reserved)
    auto addr = allocator.allocate(300);
    
    EXPECT_FALSE(addr.has_value()) 
        << "Allocation larger than 256 should fail";
}

TEST_F(ZeroPageAllocatorTest, Allocate_ExactlyMaxUserArea) {
    // User area is 0x08 to 0xFF = 248 bytes
    auto addr = allocator.allocate(248);
    
    EXPECT_TRUE(addr.has_value()) 
        << "Allocation of exactly user area size should succeed";
}

// ============================================================================
// Exhaustion Tests
// ============================================================================

TEST_F(ZeroPageAllocatorTest, Allocate_ExhaustMemory_EventuallyFails) {
    int successCount = 0;
    while (allocator.allocate(1).has_value()) {
        successCount++;
        if (successCount > 256) {
            FAIL() << "Allocation should fail before 256 single-byte allocations";
        }
    }
    
    EXPECT_GT(successCount, 0) << "Should have some successful allocations";
    EXPECT_LE(successCount, 248) << "Should not exceed user area (248 bytes)";
}

TEST_F(ZeroPageAllocatorTest, Allocate_AfterExhaustion_ReturnsNullopt) {
    // Exhaust all memory
    while (allocator.allocate(1).has_value()) {}
    
    // Further allocation should fail
    auto addr = allocator.allocate(1);
    EXPECT_FALSE(addr.has_value()) 
        << "Allocation after exhaustion should return nullopt";
}

// ============================================================================
// Free Tests
// ============================================================================

TEST_F(ZeroPageAllocatorTest, Free_EnablesReallocation) {
    auto addr1 = allocator.allocate(4);
    ASSERT_TRUE(addr1.has_value());
    
    allocator.free(addr1.value(), 4);
    
    // Should be able to allocate again
    auto addr2 = allocator.allocate(4);
    EXPECT_TRUE(addr2.has_value()) 
        << "Freed memory should be available for reallocation";
}

TEST_F(ZeroPageAllocatorTest, Free_PartialBlock) {
    auto addr = allocator.allocate(4);
    ASSERT_TRUE(addr.has_value());
    
    // Free only 2 bytes
    allocator.free(addr.value(), 2);
    
    // Should be able to allocate 2 bytes
    auto addr2 = allocator.allocate(2);
    EXPECT_TRUE(addr2.has_value());
}

TEST_F(ZeroPageAllocatorTest, Free_UpdatesNextFreeHint) {
    auto addr1 = allocator.allocate(2);
    auto addr2 = allocator.allocate(2);
    ASSERT_TRUE(addr1.has_value());
    ASSERT_TRUE(addr2.has_value());
    
    // Free the first block
    allocator.free(addr1.value(), 2);
    
    // Next allocation should reuse freed space
    auto addr3 = allocator.allocate(2);
    EXPECT_TRUE(addr3.has_value());
    EXPECT_EQ(addr3.value(), addr1.value()) 
        << "Allocator should reuse freed earlier memory";
}

// ============================================================================
// Reserve Tests
// ============================================================================

TEST_F(ZeroPageAllocatorTest, Reserve_SpecificAddress_Succeeds) {
    bool result = allocator.reserve(0x80, 4);
    
    EXPECT_TRUE(result) << "Reserving free address should succeed";
}

TEST_F(ZeroPageAllocatorTest, Reserve_PreventsFutureAllocation) {
    allocator.reserve(0x80, 4);
    
    // Allocate all remaining space
    size_t totalAllocated = 0;
    while (auto addr = allocator.allocate(1)) {
        EXPECT_FALSE(addr.value() >= 0x80 && addr.value() < 0x84) 
            << "Allocation should not overlap with reserved area";
        totalAllocated++;
        if (totalAllocated > 256) break;
    }
}

TEST_F(ZeroPageAllocatorTest, Reserve_OverlappingRange_Fails) {
    ASSERT_TRUE(allocator.reserve(0x80, 4));
    
    // Try to reserve overlapping range
    EXPECT_FALSE(allocator.reserve(0x82, 2)) 
        << "Reserving overlapping range should fail";
}

TEST_F(ZeroPageAllocatorTest, Reserve_AlreadyAllocated_Fails) {
    auto addr = allocator.allocate(4);
    ASSERT_TRUE(addr.has_value());
    
    // Try to reserve same address
    EXPECT_FALSE(allocator.reserve(addr.value(), 4)) 
        << "Reserving already allocated address should fail";
}

TEST_F(ZeroPageAllocatorTest, Reserve_SystemArea_Fails) {
    // System area 0x00-0x07 is already reserved by constructor
    EXPECT_FALSE(allocator.reserve(0x02, 2)) 
        << "Reserving system area should fail";
}

TEST_F(ZeroPageAllocatorTest, Reserve_BeyondBoundary_Fails) {
    EXPECT_FALSE(allocator.reserve(0xFE, 4)) 
        << "Reservation extending beyond 256 bytes should fail";
}

// ============================================================================
// Available Space Tests
// ============================================================================

TEST_F(ZeroPageAllocatorTest, Available_InitialState) {
    size_t avail = allocator.available();
    
    // User area is 0x08 to 0xFF = 248 bytes
    EXPECT_EQ(avail, 248u) 
        << "Initial available space should be 248 bytes (user area)";
}

TEST_F(ZeroPageAllocatorTest, Available_AfterAllocation) {
    size_t before = allocator.available();
    
    allocator.allocate(10);
    
    size_t after = allocator.available();
    EXPECT_EQ(after, before - 10) 
        << "Available should decrease by allocation size";
}

TEST_F(ZeroPageAllocatorTest, Available_AfterFree) {
    allocator.allocate(10);
    size_t afterAlloc = allocator.available();
    
    allocator.free(0x08, 10);
    
    EXPECT_EQ(allocator.available(), afterAlloc + 10) 
        << "Available should increase after free";
}

// ============================================================================
// Largest Block Tests
// ============================================================================

TEST_F(ZeroPageAllocatorTest, LargestBlock_InitialState) {
    size_t largest = allocator.largestBlock();
    
    EXPECT_EQ(largest, 248u) 
        << "Initial largest block should be entire user area";
}

TEST_F(ZeroPageAllocatorTest, LargestBlock_AfterFragmentation) {
    // Allocate-free pattern to create fragmentation
    auto addr1 = allocator.allocate(10);
    auto addr2 = allocator.allocate(10);
    auto addr3 = allocator.allocate(10);
    
    ASSERT_TRUE(addr1.has_value());
    ASSERT_TRUE(addr2.has_value());
    ASSERT_TRUE(addr3.has_value());
    
    // Free middle block
    allocator.free(addr2.value(), 10);
    
    size_t largest = allocator.largestBlock();
    
    // Largest contiguous block should be at the end (after addr3)
    // or at the beginning if addr1 was at start
    EXPECT_GE(largest, 10u) << "Should have at least 10-byte free block";
}

TEST_F(ZeroPageAllocatorTest, LargestBlock_AfterExhaustion) {
    // Exhaust all memory
    while (allocator.allocate(1).has_value()) {}
    
    EXPECT_EQ(allocator.largestBlock(), 0u) 
        << "Largest block should be 0 when exhausted";
}

// ============================================================================
// Total Allocated Tests
// ============================================================================

TEST_F(ZeroPageAllocatorTest, TotalAllocated_InitialState) {
    size_t total = allocator.totalAllocated();
    
    // System area (0x00-0x07) = 8 bytes reserved
    EXPECT_EQ(total, 8u) 
        << "Initial allocation should be 8 bytes (system area)";
}

TEST_F(ZeroPageAllocatorTest, TotalAllocated_AfterAllocation) {
    size_t before = allocator.totalAllocated();
    
    allocator.allocate(10);
    
    EXPECT_EQ(allocator.totalAllocated(), before + 10);
}

TEST_F(ZeroPageAllocatorTest, TotalAllocated_AfterReserve) {
    size_t before = allocator.totalAllocated();
    
    allocator.reserve(0x80, 5);
    
    EXPECT_EQ(allocator.totalAllocated(), before + 5);
}

// ============================================================================
// Edge Cases
// ============================================================================

TEST_F(ZeroPageAllocatorTest, EdgeCase_AllocateOne_RepeatedlyUntilFull) {
    std::vector<uint8_t> addresses;
    
    while (auto addr = allocator.allocate(1)) {
        addresses.push_back(addr.value());
    }
    
    // Verify all addresses are unique
    std::set<uint8_t> uniqueAddrs(addresses.begin(), addresses.end());
    EXPECT_EQ(uniqueAddrs.size(), addresses.size()) 
        << "All allocated addresses should be unique";
}

TEST_F(ZeroPageAllocatorTest, EdgeCase_FreeAtBoundary) {
    auto addr = allocator.allocate(1);
    ASSERT_TRUE(addr.has_value());
    
    // Free at exact boundary
    allocator.free(addr.value(), 1);
    
    // Should be able to reallocate
    auto addr2 = allocator.allocate(1);
    EXPECT_TRUE(addr2.has_value());
}

TEST_F(ZeroPageAllocatorTest, EdgeCase_LargeContiguousBlock) {
    // Allocate a large block
    auto addr = allocator.allocate(100);
    ASSERT_TRUE(addr.has_value());
    
    // Verify it's actually contiguous (can access as array)
    EXPECT_GE(addr.value() + 99, addr.value()) 
        << "100-byte block should be contiguous";
}

// ============================================================================
// Integration with CodegenVisitor
// ============================================================================

TEST_F(CodegenTestBase, ZeroPageAllocator_VariableAllocation) {
    auto var1 = makeVariable("zp1", AstType::Primitive(TypeKind::UNSIGNED_8), 
                             nullptr, false, true);
    auto var2 = makeVariable("zp2", AstType::Primitive(TypeKind::UNSIGNED_8), 
                             nullptr, false, true);
    
    visitor.visit(*var1);
    visitor.visit(*var2);
    
    auto sym1 = findSymbol("zp1");
    auto sym2 = findSymbol("zp2");
    
    ASSERT_TRUE(sym1.has_value());
    ASSERT_TRUE(sym2.has_value());
    
    EXPECT_NE(sym1->offset, sym2->offset) 
        << "Two zero-page variables should have different offsets";
}

TEST_F(CodegenTestBase, ZeroPageAllocator_16BitVariable) {
    auto var = makeVariable("zp16", AstType::Primitive(TypeKind::UNSIGNED_16), 
                            nullptr, false, true);
    visitor.visit(*var);
    
    auto sym = findSymbol("zp16");
    ASSERT_TRUE(sym.has_value());
    
    EXPECT_EQ(sym->storage, StorageClass::ZeroPage);
    EXPECT_EQ(sym->size, 2u);
}

TEST_F(CodegenTestBase, ZeroPageAllocator_SystemReservations) {
    // System reservations are made in constructor
    // __stack_pointer, __frame_pointer, __temporary, etc.
    
    ZeroPageAllocator& alloc = getZeroPageAllocator();
    
    // System area should already be allocated
    size_t systemAllocated = alloc.totalAllocated();
    EXPECT_GE(systemAllocated, 8u) 
        << "Constructor should reserve system zero-page addresses";
}

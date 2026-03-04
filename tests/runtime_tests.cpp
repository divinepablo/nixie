/**
 * @file runtime_tests.cpp
 * @brief Runtime emulation tests for the Nixie language
 *
 * All tests exercise the full compiler pipeline:
 *   AST → CodegenVisitor → O65 → Linker → flat binary → vrEmu6502 execution
 *   → register/memory assertions
 *
 * These tests validate that the Nixie language compiles to correct, runnable
 * 65C02 code — not that the emulator can execute hand-crafted opcodes.
 */

#include "emu_test_helpers.hpp"
#include "opcodes.h"

// ============================================================================
// EMULATOR SANITY CHECK (single test to confirm the test harness works)
// ============================================================================

class EmulatorSanityTest : public RuntimeTestBase {};

TEST_F(EmulatorSanityTest, HandCraftedNOP_BRK) {
    // Minimal test: just NOP + BRK to confirm the emulator + linker pipeline
    std::vector<uint8_t> code = { 0xEA, 0x00 }; // NOP ; BRK
    auto binary = buildO65FromBytes(code);
    loadBinary(binary, 0x8000);
    run();
    EXPECT_EQ(getPC(), 0x8001) << "PC should stop at BRK after NOP";
}

// ============================================================================
// GLOBAL VARIABLE INITIALIZATION (language-level)
// ============================================================================

class GlobalVariableTest : public RuntimeTestBase {};

TEST_F(GlobalVariableTest, U8_ConstantInit) {
    // Nixie: var x: u8 = 42
    // Global constant initializer → value baked into data segment
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("x", AstType{TypeKind::UNSIGNED_8}, makeNumber(42)));
    // Need a function so the linker has an entry point
    nodes.push_back(makeFunction("main", {}, {}));

    auto binary = compileAndLink(nodes);

    // Data segment at $0200: first byte should be 42
    EXPECT_EQ(readMem(0x0200), 42) << "Global u8 = 42 should be in data segment";
}

TEST_F(GlobalVariableTest, U16_ConstantInit) {
    // Nixie: var x: u16 = 0x1234
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("x", AstType{TypeKind::UNSIGNED_16}, makeNumber(0x1234)));
    nodes.push_back(makeFunction("main", {}, {}));

    auto binary = compileAndLink(nodes);

    // u16 is little-endian: low byte first
    EXPECT_EQ(readMem(0x0200), 0x34) << "u16 low byte";
    EXPECT_EQ(readMem(0x0201), 0x12) << "u16 high byte";
}

TEST_F(GlobalVariableTest, MultipleGlobals) {
    // Nixie: var a: u8 = 10; var b: u8 = 20; var c: u8 = 30
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("a", AstType{TypeKind::UNSIGNED_8}, makeNumber(10)));
    nodes.push_back(makeVariable("b", AstType{TypeKind::UNSIGNED_8}, makeNumber(20)));
    nodes.push_back(makeVariable("c", AstType{TypeKind::UNSIGNED_8}, makeNumber(30)));
    nodes.push_back(makeFunction("main", {}, {}));

    auto binary = compileAndLink(nodes);

    EXPECT_EQ(readMem(0x0200), 10);
    EXPECT_EQ(readMem(0x0201), 20);
    EXPECT_EQ(readMem(0x0202), 30);
}

// ============================================================================
// ARITHMETIC (runtime computation via assignment)
// ============================================================================

class ArithmeticLangTest : public RuntimeTestBase {};

TEST_F(ArithmeticLangTest, AddTwoNumbers) {
    // Nixie: var result: u8 = 0; fn main() { result = 5 + 3 }
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("result", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeAssign(
        makeRef("result"),
        makeBinary(Operator::ADD, makeNumber(5), makeNumber(3))
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0200), 8) << "5 + 3 should be 8";
}

TEST_F(ArithmeticLangTest, SubtractTwoNumbers) {
    // Nixie: var result: u8 = 0; fn main() { result = 10 - 3 }
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("result", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeAssign(
        makeRef("result"),
        makeBinary(Operator::SUBTRACT, makeNumber(10), makeNumber(3))
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0200), 7) << "10 - 3 should be 7";
}

TEST_F(ArithmeticLangTest, ChainedAddition) {
    // Nixie: var result: u8 = 0; fn main() { result = 1 + 2 + 3 }
    // This is (1 + 2) + 3 in AST
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("result", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeAssign(
        makeRef("result"),
        makeBinary(Operator::ADD,
            makeBinary(Operator::ADD, makeNumber(1), makeNumber(2)),
            makeNumber(3))
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0200), 6) << "1 + 2 + 3 should be 6";
}

TEST_F(ArithmeticLangTest, AssignFromVariable) {
    // Nixie: var a: u8 = 10; var b: u8 = 0; fn main() { b = a }
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("a", AstType{TypeKind::UNSIGNED_8}, makeNumber(10)));
    nodes.push_back(makeVariable("b", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeAssign(makeRef("b"), makeRef("a")));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0200), 10) << "a should still be 10";
    EXPECT_EQ(readMem(0x0201), 10) << "b should be assigned a's value (10)";
}

// ============================================================================
// CONTROL FLOW (while loops, if/else)
// ============================================================================

class ControlFlowLangTest : public RuntimeTestBase {};

TEST_F(ControlFlowLangTest, WhileCountdown) {
    // Nixie:
    //   var counter: u8 = 5
    //   fn main() {
    //       while (counter != 0) {
    //           counter = counter - 1
    //       }
    //   }
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("counter", AstType{TypeKind::UNSIGNED_8}, makeNumber(5)));

    std::vector<std::unique_ptr<Node>> loopBody;
    loopBody.push_back(makeAssign(
        makeRef("counter"),
        makeBinary(Operator::SUBTRACT, makeRef("counter"), makeNumber(1))
    ));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeWhile(
        makeComp(Comparison::NOT_EQUAL, makeRef("counter"), makeNumber(0)),
        std::move(loopBody)
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0200), 0) << "Counter should be 0 after counting down from 5";
}

TEST_F(ControlFlowLangTest, IfElse_TrueBranch) {
    // Nixie:
    //   var flag: u8 = 1
    //   var result: u8 = 0
    //   fn main() {
    //       if (flag != 0) {
    //           result = 42
    //       } else {
    //           result = 99
    //       }
    //   }
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("flag", AstType{TypeKind::UNSIGNED_8}, makeNumber(1)));
    nodes.push_back(makeVariable("result", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> thenBody;
    thenBody.push_back(makeAssign(makeRef("result"), makeNumber(42)));

    std::vector<std::unique_ptr<Node>> elseBody;
    elseBody.push_back(makeAssign(makeRef("result"), makeNumber(99)));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeIf(
        makeComp(Comparison::NOT_EQUAL, makeRef("flag"), makeNumber(0)),
        std::move(thenBody),
        std::move(elseBody)
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0201), 42) << "flag=1 → true branch → result=42";
}

TEST_F(ControlFlowLangTest, IfElse_FalseBranch) {
    // Same as above but flag = 0 → else branch
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("flag", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));
    nodes.push_back(makeVariable("result", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> thenBody;
    thenBody.push_back(makeAssign(makeRef("result"), makeNumber(42)));

    std::vector<std::unique_ptr<Node>> elseBody;
    elseBody.push_back(makeAssign(makeRef("result"), makeNumber(99)));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeIf(
        makeComp(Comparison::NOT_EQUAL, makeRef("flag"), makeNumber(0)),
        std::move(thenBody),
        std::move(elseBody)
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0201), 99) << "flag=0 → else branch → result=99";
}

// ============================================================================
// FUNCTION CALLS
// ============================================================================

class FunctionCallLangTest : public RuntimeTestBase {};

TEST_F(FunctionCallLangTest, SimpleCall) {
    // Nixie:
    //   var result: u8 = 0
    //   fn set_result() { result = 42 }
    //   fn main() { set_result() }
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("result", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> helperBody;
    helperBody.push_back(makeAssign(makeRef("result"), makeNumber(42)));
    nodes.push_back(makeFunction("set_result", {}, std::move(helperBody)));

    std::vector<std::unique_ptr<Node>> mainBody;
    mainBody.push_back(makeCall("set_result"));
    nodes.push_back(makeFunction("main", {}, std::move(mainBody)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0200), 42) << "set_result() should store 42";
}

TEST_F(FunctionCallLangTest, TwoSequentialCalls) {
    // Nixie:
    //   var a: u8 = 0; var b: u8 = 0
    //   fn set_a();  fn set_b();      // forward declarations
    //   fn main() { set_a(); set_b() }
    //   fn set_a() { a = 11 }
    //   fn set_b() { b = 22 }
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("a", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));
    nodes.push_back(makeVariable("b", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    // Forward declarations so main can call them
    nodes.push_back(makeFunctionDecl("set_a", {}));
    nodes.push_back(makeFunctionDecl("set_b", {}));

    // main first so it's at the entry point ($8000)
    std::vector<std::unique_ptr<Node>> mainBody;
    mainBody.push_back(makeCall("set_a"));
    mainBody.push_back(makeCall("set_b"));
    nodes.push_back(makeFunction("main", {}, std::move(mainBody)));

    std::vector<std::unique_ptr<Node>> setABody;
    setABody.push_back(makeAssign(makeRef("a"), makeNumber(11)));
    nodes.push_back(makeFunction("set_a", {}, std::move(setABody)));

    std::vector<std::unique_ptr<Node>> setBBody;
    setBBody.push_back(makeAssign(makeRef("b"), makeNumber(22)));
    nodes.push_back(makeFunction("set_b", {}, std::move(setBBody)));

    auto binary = compileAndLink(nodes);
    run();

    EXPECT_EQ(readMem(0x0200), 11) << "set_a() should store 11";
    EXPECT_EQ(readMem(0x0201), 22) << "set_b() should store 22";
}

// ============================================================================
// FUNCTION PARAMETER PASSING
// ============================================================================

class FunctionParamTest : public RuntimeTestBase {};

TEST_F(FunctionParamTest, U8_ParamPassedCorrectly) {
    // Nixie:
    //   var result: u8 = 0
    //   fn store_val(v: u8) { result = v }
    //   fn main() { store_val(42) }
    //
    // After execution: result should be 42
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("result", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    // Forward-declare so main can call it
    nodes.push_back(makeFunctionDecl("store_val", {{"v", AstType{TypeKind::UNSIGNED_8}}}));

    // main calls store_val(42)
    std::vector<std::unique_ptr<Node>> mainBody;
    {
        std::vector<std::unique_ptr<Node>> args;
        args.push_back(makeNumber(42));
        mainBody.push_back(makeCall("store_val", std::move(args)));
    }
    nodes.push_back(makeFunction("main", {}, std::move(mainBody)));

    // fn store_val(v: u8) { result = v }
    std::vector<std::unique_ptr<Node>> storeBody;
    storeBody.push_back(makeAssign(makeRef("result"), makeRef("v")));
    nodes.push_back(makeFunction("store_val", {{"v", AstType{TypeKind::UNSIGNED_8}}}, std::move(storeBody)));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty());
    run();

    EXPECT_EQ(readMem(0x0200), 42) << "result should be 42 (passed via function parameter)";
}

TEST_F(FunctionParamTest, PutcharWritesToMemory) {
    // Mirrors test.nxe: putchar writes a u8 parameter to a global variable.
    // Nixie:
    //   var output: u8 = 0
    //   fn putchar(c: u8) { output = c }
    //   fn main() { putchar(0x48) }   // 'H' = 0x48
    //
    // After execution: output should be 0x48
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("output", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    nodes.push_back(makeFunctionDecl("putchar", {{"c", AstType{TypeKind::UNSIGNED_8}}}));

    std::vector<std::unique_ptr<Node>> mainBody;
    {
        std::vector<std::unique_ptr<Node>> args;
        args.push_back(makeNumber('H'));
        mainBody.push_back(makeCall("putchar", std::move(args)));
    }
    nodes.push_back(makeFunction("main", {}, std::move(mainBody)));

    std::vector<std::unique_ptr<Node>> putcharBody;
    putcharBody.push_back(makeAssign(makeRef("output"), makeRef("c")));
    nodes.push_back(makeFunction("putchar", {{"c", AstType{TypeKind::UNSIGNED_8}}}, std::move(putcharBody)));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty());
    run();

    EXPECT_EQ(readMem(0x0200), 'H') << "output should be 'H' (0x48) after putchar('H')";
}

TEST_F(FunctionParamTest, RepeatedCallsNoStackOverflow) {
    // Verifies caller-side stack cleanup: calling a function with parameters
    // in a loop should not overflow the stack.
    //
    // Nixie:
    //   var counter: u8 = 3
    //   var output: u8 = 0
    //   fn store_val(v: u8) { output = v }
    //   fn main() {
    //       while (counter != 0) {
    //           store_val(counter)
    //           counter = counter - 1
    //       }
    //   }
    //
    // After execution: counter=0, output=1 (last value stored before reaching 0)
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("counter", AstType{TypeKind::UNSIGNED_8}, makeNumber(3)));
    nodes.push_back(makeVariable("output", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    nodes.push_back(makeFunctionDecl("store_val", {{"v", AstType{TypeKind::UNSIGNED_8}}}));

    std::vector<std::unique_ptr<Node>> loopBody;
    {
        std::vector<std::unique_ptr<Node>> args;
        args.push_back(makeRef("counter"));
        loopBody.push_back(makeCall("store_val", std::move(args)));
    }
    loopBody.push_back(makeAssign(
        makeRef("counter"),
        makeBinary(Operator::SUBTRACT, makeRef("counter"), makeNumber(1))
    ));

    std::vector<std::unique_ptr<Node>> mainBody;
    mainBody.push_back(makeWhile(
        makeComp(Comparison::NOT_EQUAL, makeRef("counter"), makeNumber(0)),
        std::move(loopBody)
    ));
    nodes.push_back(makeFunction("main", {}, std::move(mainBody)));

    std::vector<std::unique_ptr<Node>> storeBody;
    storeBody.push_back(makeAssign(makeRef("output"), makeRef("v")));
    nodes.push_back(makeFunction("store_val", {{"v", AstType{TypeKind::UNSIGNED_8}}}, std::move(storeBody)));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty());
    run();

    EXPECT_EQ(readMem(0x0200), 0) << "counter should be 0 after loop";
    EXPECT_EQ(readMem(0x0201), 1) << "output should be 1 (last call was store_val(1))";
}

TEST_F(FunctionParamTest, PutcharWritesThroughPointer) {
    // Exactly mirrors test.nxe: putchar writes param through pointer dereference.
    // Nixie:
    //   var chrout: ptr[u8] = 0x0300
    //   fn putchar(c: u8) { @chrout = c }
    //   fn main() { putchar(0x48) }   // 'H' = 0x48
    //
    // After execution: memory at $0300 should be 'H' (0x48)
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("chrout",
        AstType::Primitive(TypeKind::UNSIGNED_8, 1),   // ptr[u8]
        makeNumber(0x0300)));

    nodes.push_back(makeFunctionDecl("putchar", {{"c", AstType{TypeKind::UNSIGNED_8}}}));

    std::vector<std::unique_ptr<Node>> mainBody;
    {
        std::vector<std::unique_ptr<Node>> args;
        args.push_back(makeNumber('H'));
        mainBody.push_back(makeCall("putchar", std::move(args)));
    }
    nodes.push_back(makeFunction("main", {}, std::move(mainBody)));

    // fn putchar(c: u8) { @chrout = c }
    std::vector<std::unique_ptr<Node>> putcharBody;
    putcharBody.push_back(makeAssign(
        makeUnary(UnaryOperator::DEREFERENCE, makeRef("chrout")),
        makeRef("c")
    ));
    nodes.push_back(makeFunction("putchar", {{"c", AstType{TypeKind::UNSIGNED_8}}}, std::move(putcharBody)));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty());
    run();

    EXPECT_EQ(readMem(0x0300), 'H')
        << "@chrout should write 'H' (0x48) to address $0300";
}

TEST_F(FunctionParamTest, PrintStringThroughPointer) {
    // Full test.nxe pattern: print iterates a string calling putchar per char.
    // Nixie:
    //   var chrout: ptr[u8] = 0x0300   // just a RAM address for test
    //   var msg: string = "AB"
    //   var count: u8 = 0
    //   fn putchar(c: u8) { @chrout = c; count = count + 1 }
    //   fn print(p: ptr[u8]) {
    //       while (@p != 0) { putchar(@p); p = p + 1 }
    //   }
    //   fn main() { print(msg) }
    //
    // After execution: count should be 2
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("chrout",
        AstType::Primitive(TypeKind::UNSIGNED_8, 1),
        makeNumber(0x0300)));
    nodes.push_back(makeVariable("msg", AstType{TypeKind::STRING}, makeString("AB")));
    nodes.push_back(makeVariable("count", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    nodes.push_back(makeFunctionDecl("putchar", {{"c", AstType{TypeKind::UNSIGNED_8}}}));
    nodes.push_back(makeFunctionDecl("print", {{"p", AstType::Primitive(TypeKind::UNSIGNED_8, 1)}}));

    // fn main() { print(msg) }
    std::vector<std::unique_ptr<Node>> mainBody;
    {
        std::vector<std::unique_ptr<Node>> args;
        args.push_back(makeRef("msg"));
        mainBody.push_back(makeCall("print", std::move(args)));
    }
    nodes.push_back(makeFunction("main", {}, std::move(mainBody)));

    // fn putchar(c: u8) { @chrout = c; count = count + 1 }
    std::vector<std::unique_ptr<Node>> putcharBody;
    putcharBody.push_back(makeAssign(
        makeUnary(UnaryOperator::DEREFERENCE, makeRef("chrout")),
        makeRef("c")
    ));
    putcharBody.push_back(makeAssign(
        makeRef("count"),
        makeBinary(Operator::ADD, makeRef("count"), makeNumber(1))
    ));
    nodes.push_back(makeFunction("putchar", {{"c", AstType{TypeKind::UNSIGNED_8}}}, std::move(putcharBody)));

    // fn print(p: ptr[u8]) { while (@p != 0) { putchar(@p); p = p + 1 } }
    std::vector<std::unique_ptr<Node>> loopBody;
    {
        std::vector<std::unique_ptr<Node>> args;
        args.push_back(makeUnary(UnaryOperator::DEREFERENCE, makeRef("p")));
        loopBody.push_back(makeCall("putchar", std::move(args)));
    }
    loopBody.push_back(makeAssign(
        makeRef("p"),
        makeBinary(Operator::ADD, makeRef("p"), makeNumber(1))
    ));
    std::vector<std::unique_ptr<Node>> printBody;
    printBody.push_back(makeWhile(
        makeComp(Comparison::NOT_EQUAL,
                 makeUnary(UnaryOperator::DEREFERENCE, makeRef("p")),
                 makeNumber(0)),
        std::move(loopBody)
    ));
    nodes.push_back(makeFunction("print", {{"p", AstType::Primitive(TypeKind::UNSIGNED_8, 1)}}, std::move(printBody)));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty());
    run();

    // count lives after chrout(2 bytes) + msg(2 bytes) + "AB\0"(3 bytes) = offset 7
    // → $0200 + 7 = $0207
    uint16_t countAddr = 0x0207;
    EXPECT_EQ(readMem(countAddr), 2)
        << "count should be 2 after print(\"AB\")";
}

// ============================================================================
// INTERRUPT HANDLER (verifies RTI is emitted)
// ============================================================================

class InterruptLangTest : public RuntimeTestBase {};

TEST_F(InterruptLangTest, IRQ_EmitsRTI) {
    // Nixie: fn handler() interrupt(irq) { }
    // Should end with RTI (0x40), not RTS (0x60)
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeFunction("handler", {}, {}, InterruptType::IRQ));
    nodes.push_back(makeFunction("main", {}, {}));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty());

    // Search text segment for RTI opcode (0x40) — it should be present
    bool foundRTI = false;
    for (size_t i = 0; i < binary.size(); ++i) {
        if (binary[i] == 0x40) { foundRTI = true; break; }
    }
    EXPECT_TRUE(foundRTI) << "IRQ handler should contain RTI opcode (0x40)";

    // Also verify main ends with RTS (0x60)
    bool foundRTS = false;
    for (size_t i = 0; i < binary.size(); ++i) {
        if (binary[i] == 0x60) { foundRTS = true; break; }
    }
    EXPECT_TRUE(foundRTS) << "main function should contain RTS opcode (0x60)";
}

// ============================================================================
// STRING / PUTS — iteration through the compiler pipeline
// ============================================================================

class StringIterationTest : public RuntimeTestBase {};

TEST_F(StringIterationTest, PutsCountsStringLength) {
    // Nixie (conceptual):
    //   var mystr: string = "Hello"
    //   var count: u8 = 0
    //   fn main() {
    //       while (@mystr != 0) {
    //           count = count + 1
    //           mystr = mystr + 1
    //       }
    //   }
    //
    // After execution, count should be 5 ("Hello" = 5 chars).
    // This exercises: string storage, pointer dereference, pointer
    // arithmetic, while loop, comparison, and assignment — all through
    // the real Nixie compiler.

    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("mystr", AstType{TypeKind::STRING}, makeString("Hello")));
    nodes.push_back(makeVariable("count", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> loopBody;
    // count = count + 1
    loopBody.push_back(makeAssign(
        makeRef("count"),
        makeBinary(Operator::ADD, makeRef("count"), makeNumber(1))
    ));
    // mystr = mystr + 1  (advance pointer)
    loopBody.push_back(makeAssign(
        makeRef("mystr"),
        makeBinary(Operator::ADD, makeRef("mystr"), makeNumber(1))
    ));

    std::vector<std::unique_ptr<Node>> body;
    // while (@mystr != 0) { ... }
    body.push_back(makeWhile(
        makeComp(Comparison::NOT_EQUAL,
                 makeUnary(UnaryOperator::DEREFERENCE, makeRef("mystr")),
                 makeNumber(0)),
        std::move(loopBody)
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty()) << "Compilation should succeed";
    run();

    // Data layout: mystr (2 bytes at $0200), "Hello\0" (6 bytes at $0202), count (1 byte at $0208)
    EXPECT_EQ(readMem(0x0208), 5) << "count should be 5 after iterating 'Hello'";
}

TEST_F(StringIterationTest, PutsEmptyString) {
    // Same pattern but with "" — count should remain 0
    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("mystr", AstType{TypeKind::STRING}, makeString("")));
    nodes.push_back(makeVariable("count", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> loopBody;
    loopBody.push_back(makeAssign(
        makeRef("count"),
        makeBinary(Operator::ADD, makeRef("count"), makeNumber(1))
    ));
    loopBody.push_back(makeAssign(
        makeRef("mystr"),
        makeBinary(Operator::ADD, makeRef("mystr"), makeNumber(1))
    ));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeWhile(
        makeComp(Comparison::NOT_EQUAL,
                 makeUnary(UnaryOperator::DEREFERENCE, makeRef("mystr")),
                 makeNumber(0)),
        std::move(loopBody)
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    run();

    // Data layout: mystr (2 bytes), "\0" (1 byte), count (1 byte at $0203)
    EXPECT_EQ(readMem(0x0203), 0) << "Empty string → count stays 0";
}

TEST_F(StringIterationTest, PutsCopiesStringToBuffer) {
    // Simulate puts: copy string bytes to a known memory-mapped output buffer.
    // Uses pointer dereference to read each char, and dereference assignment
    // to write through an output pointer.
    //
    // Nixie (conceptual):
    //   var src: string = "AB"
    //   var dst: u16 = 0x0300      // output buffer address (absolute)
    //   var count: u8 = 0
    //   fn main() {
    //       while (@src != 0) {
    //           @dst = @src         // copy char
    //           src = src + 1
    //           dst = dst + 1
    //           count = count + 1
    //       }
    //   }

    std::vector<std::unique_ptr<Node>> nodes;
    nodes.push_back(makeVariable("src", AstType{TypeKind::STRING}, makeString("AB")));
    nodes.push_back(makeVariable("dst", AstType{TypeKind::UNSIGNED_16}, makeNumber(0x0300)));
    nodes.push_back(makeVariable("count", AstType{TypeKind::UNSIGNED_8}, makeNumber(0)));

    std::vector<std::unique_ptr<Node>> loopBody;
    // @dst = @src
    loopBody.push_back(makeAssign(
        makeUnary(UnaryOperator::DEREFERENCE, makeRef("dst")),
        makeUnary(UnaryOperator::DEREFERENCE, makeRef("src"))
    ));
    // src = src + 1
    loopBody.push_back(makeAssign(
        makeRef("src"),
        makeBinary(Operator::ADD, makeRef("src"), makeNumber(1))
    ));
    // dst = dst + 1
    loopBody.push_back(makeAssign(
        makeRef("dst"),
        makeBinary(Operator::ADD, makeRef("dst"), makeNumber(1))
    ));
    // count = count + 1
    loopBody.push_back(makeAssign(
        makeRef("count"),
        makeBinary(Operator::ADD, makeRef("count"), makeNumber(1))
    ));

    std::vector<std::unique_ptr<Node>> body;
    body.push_back(makeWhile(
        makeComp(Comparison::NOT_EQUAL,
                 makeUnary(UnaryOperator::DEREFERENCE, makeRef("src")),
                 makeNumber(0)),
        std::move(loopBody)
    ));
    nodes.push_back(makeFunction("main", {}, std::move(body)));

    auto binary = compileAndLink(nodes);
    ASSERT_FALSE(binary.empty());
    run();

    // Verify output buffer at $0300 has "AB"
    EXPECT_EQ(readMem(0x0300), 'A') << "First byte of output should be 'A'";
    EXPECT_EQ(readMem(0x0301), 'B') << "Second byte of output should be 'B'";

    // count is at data offset: src(2) + "AB\0"(3) + dst(2) + count(1) = offset 7
    // → $0200 + 7 = $0207
    EXPECT_EQ(readMem(0x0207), 2) << "count should be 2";
}

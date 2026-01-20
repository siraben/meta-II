#!/usr/bin/env bash
# Test suite for META II compiler-compiler

# Don't exit on first error - we want to run all tests

echo "=== META II Test Suite ==="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

PASS=0
FAIL=0

pass() {
    echo -e "${GREEN}PASS${NC}: $1"
    ((PASS++))
}

fail() {
    echo -e "${RED}FAIL${NC}: $1"
    ((FAIL++))
}

# Test 1: Build the VM
echo "Test 1: Building VM..."
if make clean > /dev/null 2>&1 && make all > /dev/null 2>&1; then
    pass "VM builds successfully"
else
    fail "VM build failed"
    exit 1
fi

# Test 2: Assemble meta-II.masm to bytecode
echo "Test 2: Assembling meta-II.masm..."
guile -c '
(load "assembler.scm")
(compile-masm-to-bytecode "meta-II.masm" "meta-II.img")
' 2>&1 | grep -q "Pass two"
if [ -f meta-II.img ] && [ -s meta-II.img ]; then
    pass "Assembly completed"
else
    fail "Assembly failed"
fi

# Test 3: Compile arith.meta
echo "Test 3: Compiling arith.meta..."
if ./vm meta-II.img arith.meta > arith.masm.out 2>&1 && grep -q "ADR EX1" arith.masm.out; then
    pass "arith.meta compiles successfully"
else
    fail "arith.meta compilation failed"
fi

# Test 4: META II self-compilation
echo "Test 4: META II self-compilation..."
if ./vm meta-II.img meta-II.meta > meta-II.self.out 2>&1 && grep -q "ADR PROGRAM" meta-II.self.out; then
    pass "META II self-compiles successfully"
else
    fail "META II self-compilation failed"
fi

# Test 5: Compare self-compiled output has expected structure
echo "Test 5: Verifying self-compiled output structure..."
expected_rules="OUT1 OUTPUT EX3 EX2 EX1 ST PROGRAM"
all_found=true
for rule in $expected_rules; do
    if ! grep -q "^$rule$" meta-II.self.out; then
        all_found=false
        echo "  Missing rule: $rule"
    fi
done
if $all_found; then
    pass "All expected rules found in self-compiled output"
else
    fail "Some rules missing from self-compiled output"
fi

# Test 6: Compile VALGOL-I
echo "Test 6: Compiling valgol-I.meta..."
if [ -f valgol-I.meta ]; then
    if ./vm meta-II.img valgol-I.meta > valgol-I.masm.out 2>&1 && grep -q "Done." valgol-I.masm.out; then
        pass "valgol-I.meta compiles successfully"
    else
        fail "valgol-I.meta compilation failed"
    fi
else
    echo "SKIP: valgol-I.meta not found"
fi

# Test 7: Compile VALGOL-II
echo "Test 7: Compiling valgol-II.meta..."
if [ -f valgol-II.meta ]; then
    if ./vm meta-II.img valgol-II.meta > valgol-II.masm.out 2>&1 && grep -q "Done." valgol-II.masm.out; then
        pass "valgol-II.meta compiles successfully"
    else
        fail "valgol-II.meta compilation failed"
    fi
else
    echo "SKIP: valgol-II.meta not found"
fi

# Clean up test outputs
rm -f arith.masm.out meta-II.self.out valgol-I.masm.out valgol-II.masm.out

echo
echo "=== Test Results ==="
echo -e "${GREEN}Passed: $PASS${NC}"
echo -e "${RED}Failed: $FAIL${NC}"

if [ $FAIL -eq 0 ]; then
    echo -e "\n${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed.${NC}"
    exit 1
fi

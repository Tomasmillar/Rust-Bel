#!/usr/bin/env bash
# run_tests.sh - Run Bel test suite and verify results
# Usage: ./tests/run_tests.sh [bel-exe] [bel-file] [test-file]

set -euo pipefail

BEL_EXE="${1:-./target/release/bel}"
BEL_FILE="${2:-bel.bel}"
TEST_FILE="${3:-./tests/test_guide_simple.bel}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
GRAY='\033[0;90m'
NC='\033[0m'

# Extract expected results
mapfile -t expectations < <(sed -n 's/^[[:space:]]*;[[:space:]]*expect:[[:space:]]*//p' "$TEST_FILE" | sed 's/[[:space:]]*$//')

# Extract expressions (non-comment, non-blank lines)
mapfile -t expressions < <(grep -Ev '^[[:space:]]*;|^[[:space:]]*$' "$TEST_FILE")

echo -e "${CYAN}Running ${#expectations[@]} tests...${NC}"
echo "  Interpreter: $BEL_EXE"
echo "  Loading:     $BEL_FILE"
echo "  Test file:   $TEST_FILE"
echo ""

# Run the interpreter
mapfile -t results < <("$BEL_EXE" --load "$BEL_FILE" < "$TEST_FILE" 2>/dev/null)

pass=0
fail=0

for i in "${!expectations[@]}"; do
    expected="${expectations[$i]}"

    if [ "$i" -ge "${#results[@]}" ]; then
        echo -e "${RED}FAIL [$((i+1))] ${expressions[$i]}${NC}"
        echo -e "${RED}       expected: ${expected}${NC}"
        echo -e "${RED}       got:      (no output)${NC}"
        ((fail++))
        continue
    fi

    actual=$(echo "${results[$i]}" | sed 's/\s*$//')

    if [ "$actual" = "$expected" ]; then
        ((pass++))
    else
        echo -e "${RED}FAIL [$((i+1))] ${expressions[$i]}${NC}"
        echo -e "${YELLOW}       expected: ${expected}${NC}"
        echo -e "${YELLOW}       got:      ${actual}${NC}"
        ((fail++))
    fi
done

# Check for extra output
if [ "${#results[@]}" -gt "${#expectations[@]}" ]; then
    echo ""
    extra=$((${#results[@]} - ${#expectations[@]}))
    echo -e "${YELLOW}WARNING: ${extra} extra output lines${NC}"
fi

# Summary
echo ""
total=$((pass + fail))
if [ "$fail" -eq 0 ]; then
    echo -e "${GREEN}Results: ${pass}/${total} passed${NC}"
    echo -e "${GREEN}All tests passed!${NC}"
else
    echo -e "${RED}Results: ${pass} passed, ${fail} failed out of ${total}${NC}"
fi

exit "$fail"

#!/bin/bash

# Summarize test results
# Usage: verdict.sh [test]

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BASE_DIR="$ROOT/RV32I/tb/tests"
OUT_DIR="$ROOT/out/tests"
PATTERN="${1:-*test}"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo ""
echo "============================================================"
echo "TEST RESULTS SUMMARY"
echo "============================================================"
printf "%-30s %-10s %s\n" "Test Name" "Verdict" "IPC"
echo "------------------------------------------------------------"

PASS=0
FAIL=0
MISSING=0

# Find all test directories
for test_dir in "$BASE_DIR"/$PATTERN/; do
    if [ -d "$test_dir" ]; then
        test_name=$(basename "$test_dir")
        result_file="$OUT_DIR/$test_name/comparison_results.txt"
        ipc=$(awk '/^ipc/{print $2}' "$OUT_DIR/$test_name/ipc.txt" 2>/dev/null)
        
        if [ -f "$result_file" ]; then
            verdict=$(grep -E "VERDICT:" "$result_file" | head -1 | awk '{print $2}')
            
            if [ -z "$verdict" ]; then
                if grep -q "FAIL: 0" "$result_file"; then
                    verdict="PASS"
                elif grep -q "FAIL:" "$result_file" && ! grep -q "FAIL: 0" "$result_file"; then
                    verdict="FAIL"
                else
                    verdict="UNKNOWN"
                fi
            fi
            
            case "$verdict" in
                PASS)
                    printf "%-30s ${GREEN}%-10s${NC} %s\n" "$test_name" "$verdict" "$ipc"
                    ((PASS++))
                    ;;
                FAIL)
                    printf "%-30s ${RED}%-10s${NC} %s\n" "$test_name" "$verdict" "$ipc"
                    ((FAIL++))
                    ;;
                *)
                    printf "%-30s ${YELLOW}%-10s${NC}\n" "$test_name" "$verdict"
                    ((MISSING++))
                    ;;
            esac
        else
            printf "%-30s ${YELLOW}%-10s${NC}\n" "$test_name" "MISSING"
            ((MISSING++))
        fi
    fi
done

echo "============================================================"

[ $FAIL -eq 0 ] && [ $MISSING -eq 0 ]

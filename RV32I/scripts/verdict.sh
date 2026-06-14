#!/bin/bash

# ======================================================
# SCRIPT: collect_results.sh
# Reads all test results and prints summary
# ======================================================

BASE_DIR="RV32I/RISCV_tb"

# Boje (opciono, ako terminal podržava)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo ""
echo "============================================================"
echo "TEST RESULTS SUMMARY"
echo "============================================================"
printf "%-30s %-10s\n" "Test Name" "Verdict"
echo "------------------------------------------------------------"

PASS=0
FAIL=0
MISSING=0

# Pronađi sve test direktorijume
for test_dir in "$BASE_DIR"/*test/; do
    if [ -d "$test_dir" ]; then
        test_name=$(basename "$test_dir")
        result_file="$test_dir/comparison_results.txt"
        
        if [ -f "$result_file" ]; then
            # Ekstraktuj VERDICT liniju
            verdict=$(grep -E "VERDICT:" "$result_file" | head -1 | awk '{print $2}')
            
            # Ako nema VERDICT linije, pokušaj drugu metodu
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
                    printf "%-30s ${GREEN}%-10s${NC}\n" "$test_name" "$verdict"
                    ((PASS++))
                    ;;
                FAIL)
                    printf "%-30s ${RED}%-10s${NC}\n" "$test_name" "$verdict"
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

#echo "------------------------------------------------------------"
#echo ""
#echo "TOTAL:     $((PASS + FAIL + MISSING)) tests"
#echo -e "${GREEN}PASS:      $PASS${NC}"
#echo -e "${RED}FAIL:      $FAIL${NC}"
#echo -e "${YELLOW}MISSING:   $MISSING${NC}"
echo "============================================================"
#echo ""


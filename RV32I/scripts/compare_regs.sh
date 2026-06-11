#!/bin/bash

# ======================================================
# REGISTER COMPARISON - fix for hex format in rename table
# ======================================================

TEST_NAME=$1

GOLDEN_REGS_FILE="../RISCV_tb/$TEST_NAME/golden_vector_regs.txt"
SIM_REGS_FILE="../RISCV_tb/$TEST_NAME/sim_vector_regs.txt"

RESULT_FILE="../RISCV_tb/$TEST_NAME/comparison_results.txt"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

if [ ! -f "$GOLDEN_REGS_FILE" ]; then
    echo -e "${RED}Golden file missing: $GOLDEN_REGS_FILE${NC}"
    exit 1
fi
if [ ! -f "$SIM_REGS_FILE" ]; then
    echo -e "${RED}Dump file missing: $SIM_REGS_FILE${NC}"
    exit 1
fi

# ======================================================
# 1. Extract rename table (arch -> physical number in decimal)
#    Physical number in dump is hexadecimal (e.g. reg_0c)
# ======================================================
grep -E "x[0-9]+[[:space:]]+->" "$SIM_REGS_FILE" | sed -E 's/x([0-9]+).*reg_([0-9a-fA-F]+).*/\1 \2/' | while read arch phys_hex; do
    # Convert hex to decimal (e.g. 0c -> 12, 0f -> 15, 13 -> 19)
    phys_dec=$((16#$phys_hex))
    echo "$arch $phys_dec"
done > /tmp/rename_map.txt


# ======================================================
# 2. Extract physical register values (indices are decimal)
# ======================================================
grep -E "reg_[0-9]+ =" "$SIM_REGS_FILE" | sed -E 's/reg_([0-9]+)[[:space:]]*=[[:space:]]*([0-9a-fA-F]+).*/\1 \2/' | while read phys_num val; do
    echo "$phys_num $(echo $val | tr '[:lower:]' '[:upper:]')"
done > /tmp/phys_vals.txt


# ======================================================
# 3. Extract expected values from golden model
# ======================================================
grep "## expect\[" "$GOLDEN_REGS_FILE" | sed -E 's/.*expect\[([0-9]+)\] = 0x([0-9a-fA-F]+).*/\1 \2/' | while read reg val; do
    echo "$reg $(echo $val | tr '[:lower:]' '[:upper:]')"
done > /tmp/golden_vals.txt

# ======================================================
# 4. Compare for all architectural registers (0-31)
# ======================================================
printf "\n%-4s %-12s %-12s %s\n" "Reg" "Expected" "Simulated" "Status"
echo "------------------------------------------------"

PASS=0
FAIL=0
FAILED_REGS=""

for arch in {0..31}; do
    # Get physical register (decimal) from rename table (default 0)
    phys=$(grep "^$arch " /tmp/rename_map.txt | cut -d' ' -f2)
    if [ -z "$phys" ]; then
        phys=0
    fi
    
    # Get physical register value (default 00000000)
    sim=$(grep "^$phys " /tmp/phys_vals.txt | cut -d' ' -f2)
    if [ -z "$sim" ]; then
        sim="00000000"
    fi
    
    # Get expected value (default 00000000)
    exp=$(grep "^$arch " /tmp/golden_vals.txt | cut -d' ' -f2)
    if [ -z "$exp" ]; then
        exp="00000000"
    fi
    
    # Comparison
    if [ "$sim" == "$exp" ]; then
        printf "x%-2d   0x%-10s 0x%-10s ${GREEN}%s${NC}\n" "$arch" "$exp" "$sim" "PASS"
        ((PASS++))
    else
        printf "x%-2d   0x%-10s 0x%-10s ${RED}%s${NC} (phys reg %d)\n" "$arch" "$exp" "$sim" "FAIL" "$phys"
        ((FAIL++))
        FAILED_REGS="$FAILED_REGS x$arch"
    fi
done

echo "------------------------------------------------"
echo -e "\n${YELLOW}PASS: $PASS, FAIL: $FAIL, TOTAL: 32${NC}"

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}VERDICT: PASS ✓${NC}"
    echo "VERDICT: PASS" > $RESULT_FILE
else
    echo -e "${RED}VERDICT: FAIL ✗${NC}"
    echo "VERDICT: FAIL" > $RESULT_FILE
    echo "Failed registers:$FAILED_REGS" >> $RESULT_FILE
fi

# Save complete results to file
{
    if [ $FAIL -eq 0 ]; then
        echo "VERDICT: PASS"
    else
        echo "VERDICT: FAIL"
        echo "Failed registers:$FAILED_REGS"
    fi
    echo "=========================================="
    echo "REGISTER COMPARISON RESULTS"
    echo "Time: $(date)"
    echo "=========================================="
    echo ""
    printf "%-4s %-12s %-12s %s\n" "Reg" "Expected" "Simulated" "Status"
    echo "------------------------------------------------"
    for arch in {0..31}; do
        phys=$(grep "^$arch " /tmp/rename_map.txt | cut -d' ' -f2)
        [ -z "$phys" ] && phys=0
        sim=$(grep "^$phys " /tmp/phys_vals.txt | cut -d' ' -f2)
        [ -z "$sim" ] && sim="00000000"
        exp=$(grep "^$arch " /tmp/golden_vals.txt | cut -d' ' -f2)
        [ -z "$exp" ] && exp="00000000"
        if [ "$sim" == "$exp" ]; then
            printf "x%-2d   0x%-10s 0x%-10s %s\n" "$arch" "$exp" "$sim" "PASS"
        else
            printf "x%-2d   0x%-10s 0x%-10s %s (phys reg %d)\n" "$arch" "$exp" "$sim" "FAIL" "$phys"
        fi
    done
    echo "------------------------------------------------"
    echo "PASS: $PASS, FAIL: $FAIL, TOTAL: 32"
} > "$RESULT_FILE"

# Clean temporary files
rm -f /tmp/rename_map.txt /tmp/phys_vals.txt /tmp/golden_vals.txt

echo ""
echo -e "${YELLOW}Detailed report saved to: $RESULT_FILE${NC}"
exit $FAIL
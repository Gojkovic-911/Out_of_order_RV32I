#!/bin/bash

# ======================================================
# CPU REGRESSION MASTER SCRIPT
# Runs simulation, generates golden vectors, compares results
# ======================================================

export TEST_NAME=$1
export TEST_DIR=RV32I/RISCV_tb/$TEST_NAME

export GOLDEN_REGS_FILE="$TEST_DIR/golden_vector_regs.txt"
export SIM_REGS_FILE="$TEST_DIR/sim_vector_regs.txt"

# Tools
#BIN_CONVERSION_SCRIPT="RV32I/scripts/convert_bin.sh"
COMPARE_SCRIPT="RV32I/scripts/compare_regs.sh"

VIVADO=$(locate bin/vivado | grep '/bin/vivado$')

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=========================================="
echo "   CPU REGRESSION / REGISTER CHECK"
echo "   TEST: $TEST_NAME"
echo "=========================================="

# ======================================================
# STEP 1: Convert binaries (if required)
# ======================================================
echo -e "${YELLOW}[1/3] Preparing binary inputs...${NC}"

riscv64-unknown-elf-as -march=rv32i $TEST_DIR/$TEST_NAME.s -o prog.o && riscv64-unknown-elf-objdump -d prog.o | awk '/^ +[0-9a-f]+:/{print $2}' > prog.hex && rm prog.o
perl -ne 'chomp; printf "%032b\n", hex($_)' < prog.hex > RV32I/RISCV_tb/assembly_code_bin.txt && rm prog.hex

# ======================================================
# STEP 2: Run Vivado simulation
# ======================================================
echo -e "${YELLOW}[2/3] Running Vivado simulation...${NC}"

$VIVADO -mode batch \
    -source RV32I/vivado/RISCV.tcl \
    -tclargs "$TEST_NAME" \
    -nojournal -nolog

echo -e "${GREEN}Simulation completed. Output: $SIM_DUMP_FILE${NC}"

# ======================================================
# STEP 3: Generate golden reference
# ======================================================
echo -e "${YELLOW}[3/3] Generating golden reference vectors...${NC}"
python3 RV32I/scripts/get_golden_vector.py "$TEST_NAME"

# ======================================================
# STEP 4: Compare results
# ======================================================
echo -e "${YELLOW}Comparing simulation vs golden...${NC}"
$COMPARE_SCRIPT
COMPARE_EXIT=$?

# ======================================================
# Cleanup temporary Vivado files
# ======================================================
rm -rf .Xil *.jou *.log

exit $COMPARE_EXIT
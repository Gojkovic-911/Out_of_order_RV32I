#!/bin/bash

# ======================================================
# MASTER SCRIPT - runs Vivado, then calls comparison script
# ======================================================

# Configuration
VIVADO_PROJECT="RV32I/RISCV_project/RISCV_project.xpr"
BIN_CONVERSION_SCRIPT="RV32I/scripts/convert_bin.sh"

export GOLDEN_FILE="RV32I/scripts/golden_vector_regs.txt"
export SIM_DUMP_FILE="RV32I/scripts/register_dump.txt"
COMPARE_SCRIPT="RV32I/scripts/compare_regs.sh"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=========================================="
echo "CPU REGISTER VERIFICATION (MASTER)"
echo "=========================================="

# ======================================================
# STEP 1: Run Vivado simulation
# ======================================================
echo -e "${YELLOW}[1/2] Starting Vivado...${NC}"

$BIN_CONVERSION_SCRIPT

~/Public/Vivado/2023.2/bin/vivado -mode batch -source RISCV.tcl -nojournal -nolog

echo -e "${GREEN}Vivado simulation completed. Dump file: $SIM_DUMP_FILE${NC}"

# ======================================================
# STEP 2: Call comparison script
# ======================================================
echo -e "${YELLOW}[2/2] Running comparison script...${NC}"

# Invoke the comparison script (assumes it reads the same files)
$COMPARE_SCRIPT

COMPARE_EXIT=$?

exit $COMPARE_EXIT
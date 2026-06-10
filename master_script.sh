#!/bin/bash

# ======================================================
# MASTER SCRIPT - runs Vivado, then calls comparison script
# ======================================================

# Configuration
GOLDEN_FILE="registers.txt"
SIM_DUMP_FILE="register_dump.txt"
VIVADO_PROJECT="RISCV_project/RISCV_project.xpr"
TCL_DUMP_SCRIPT="reg_dump.tcl"

# Path to comparison script (change if needed)
COMPARE_SCRIPT="./compare_script.sh"

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
echo -e "${YELLOW}[1/2] Starting Vivado simulation...${NC}"

cat > run_sim.tcl << EOF
open_project $VIVADO_PROJECT
launch_simulation
run 50 us
source $TCL_DUMP_SCRIPT
close_sim
exit
EOF

~/Public/Vivado/2023.2/bin/vivado -mode batch -source run_sim.tcl -nojournal -nolog

if [ $? -ne 0 ]; then
    echo -e "${RED}Error: Vivado simulation failed!${NC}"
    exit 1
fi

rm -f run_sim.tcl
echo -e "${GREEN}Vivado simulation completed. Dump file: $SIM_DUMP_FILE${NC}"

# ======================================================
# STEP 2: Call comparison script
# ======================================================
echo -e "${YELLOW}[2/2] Running comparison script...${NC}"

if [ ! -f "$COMPARE_SCRIPT" ]; then
    echo -e "${RED}Comparison script not found: $COMPARE_SCRIPT${NC}"
    exit 1
fi

# Invoke the comparison script (assumes it reads the same files)
$COMPARE_SCRIPT

COMPARE_EXIT=$?

if [ $COMPARE_EXIT -eq 0 ]; then
    echo -e "${GREEN}All OK. Registers match.${NC}"
else
    echo -e "${RED}Comparison detected differences.${NC}"
fi

exit $COMPARE_EXIT
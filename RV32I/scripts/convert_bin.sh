#!/bin/bash

# ======================================================
# IZDVAJANJE BINARNIH VRIJEDNOSTI IZ LISTE INSTUKCIJA
# ======================================================

INPUT_FILE="RV32I/RISCV_tb/assembly_code.txt"
OUTPUT_FILE="RV32I/RISCV_tb/assembly_code_bin.txt"

# Pronađi sve 32-bitne binarne nizove (samo 0 i 1, tačno 32 karaktera)
grep -Eo '\b[01]{32}\b' "$INPUT_FILE" > "$OUTPUT_FILE"

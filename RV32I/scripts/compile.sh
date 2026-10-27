#!/bin/bash

TEST_NAME=$1

ASM_FILE="../tests/$TEST_NAME/$TEST_NAME.s"
BIN_FILE="../tests/assembly_code_bin.txt"
DIS_FILE="../tests/$TEST_NAME/$TEST_NAME.dis"

echo "Compiling: $TEST_NAME"

sed -i -z '$s/\n*$/\n/' "$ASM_FILE"

# Assemble and convert
riscv64-unknown-elf-as -march=rv32i "$ASM_FILE" -o "prog.o"

riscv64-unknown-elf-objdump -d "prog.o" | awk '/^ +[0-9a-f]+:/{print $2}' > "prog.hex"

perl -ne 'chomp; printf "%032b\n", hex($_)' < "prog.hex" > "$BIN_FILE"

# Disassembly with x0-x31 names
riscv64-unknown-elf-objdump -d "prog.o" | \
    grep -v '^[0-9a-f]\+:' | \
    sed 's/zero/x0/g; s/ra/x1/g; s/sp/x2/g; s/gp/x3/g; s/tp/x4/g; s/t0/x5/g; s/t1/x6/g; s/t2/x7/g; s/s0/x8/g; s/fp/x8/g; s/s1/x9/g; s/a0/x10/g; s/a1/x11/g; s/a2/x12/g; s/a3/x13/g; s/a4/x14/g; s/a5/x15/g; s/a6/x16/g; s/a7/x17/g; s/s2/x18/g; s/s3/x19/g; s/s4/x20/g; s/s5/x21/g; s/s6/x22/g; s/s7/x23/g; s/s8/x24/g; s/s9/x25/g; s/s10/x26/g; s/s11/x27/g; s/t3/x28/g; s/t4/x29/g; s/t5/x30/g; s/t6/x31/g' > "$DIS_FILE"

sed -i -z '$s/\n*$/\n/' "$DIS_FILE"

rm -f "prog.hex" "prog.o"

echo "Done!"
echo "  - $BIN_FILE"
echo "  - $DIS_FILE"
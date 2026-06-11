#!/bin/bash

# ======================================================
# EXTRACT BINARY VALUES FROM INSTRUCTION LIST
# ======================================================

input_file="RV32I/RISCV_tb/"$TEST_NAME"/"$TEST_NAME".dis"
OUTPUT_FILE="RV32I/RISCV_tb/assembly_code_bin.txt"

# Check if input file exists
if [ ! -f "$input_file" ]; then
    echo "ERROR: File '$input_file' does not exist!"
    exit 1
fi

# Function: convert hexadecimal number to 32-bit binary string (pure Bash)
hex_to_bin32() {
    local hex="$1"
    local dec=$((16#$hex))  # convert hex to decimal
    local bin=""
    for ((i=31; i>=0; i--)); do
        if (( (dec >> i) & 1 )); then
            bin="${bin}1"
        else
            bin="${bin}0"
        fi
    done
    echo "$bin"
}

# Extract instructions using sed:
# - look for lines starting with (optional whitespace) + hex address + colon
# - then whitespace + exactly 8 hex digits
# - print only those 8 hex digits
sed -n 's/^[[:space:]]*[0-9a-fA-F]\+:[[:space:]]\+\([0-9a-fA-F]\{8\}\).*/\1/p' "$input_file" | while read hex; do
    hex_low=$(echo "$hex" | tr '[:upper:]' '[:lower:]')
    hex_to_bin32 "$hex_low"
done > "$OUTPUT_FILE"

echo ""
echo "Conversion finished. Binary file: $OUTPUT_FILE"
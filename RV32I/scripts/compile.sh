TEST_NAME=$1
riscv64-unknown-elf-as -march=rv32i ../RISCV_tb/$TEST_NAME/$TEST_NAME.s -o prog.o && riscv64-unknown-elf-objdump -d prog.o | awk '/^ +[0-9a-f]+:/{print $2}' > prog.hex
perl -ne 'chomp; printf "%032b\n", hex($_)' < prog.hex > ../RISCV_tb/assembly_code_bin.txt && rm -f prog.hex prog.o

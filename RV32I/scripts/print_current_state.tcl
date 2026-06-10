# reg_dump.tcl
# Open file for writing
set dump_file [open "../scripts/register_dump.txt" w]

# ===========================================
# PHYSICAL REGISTERS
# ===========================================
puts $dump_file "========================================="
puts $dump_file "PHYSICAL REGISTERS (phys_regs_s)"
puts $dump_file "========================================="

for {set i 0} {$i < 64} {incr i} {
    set reg_val [get_value /TOP_RISCV_tb/TOP_RISCV_1/data_path_1/phys_regs_s($i)]
    puts $dump_file "reg_$i = $reg_val"
}

puts $dump_file ""
puts $dump_file "========================================="
puts $dump_file "COMMIT RENAME TABLE (commit_rename_table_s)"
puts $dump_file "========================================="
puts $dump_file "Arch reg -> Physical reg"
puts $dump_file "-----------------------------------------"

for {set i 0} {$i < 32} {incr i} {
    set phys_reg [get_value /TOP_RISCV_tb/TOP_RISCV_1/data_path_1/u_renaming_module/commit_rename_table_s($i)]
    puts $dump_file "x$i       -> reg_$phys_reg"
}

puts $dump_file "========================================="

# Close the file
close $dump_file

# Console confirmation
puts "Dump completed! Data is in file: register_dump.txt"
puts "Location: [pwd]/register_dump.txt"

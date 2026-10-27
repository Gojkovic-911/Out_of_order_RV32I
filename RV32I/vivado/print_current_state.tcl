# Dump physical registers

set testname [lindex $argv 0]

set dump_file [open "../tests/$::test/sim_vector_regs.txt" w]

puts $dump_file "========================================="
puts $dump_file "PHYSICAL REGISTERS (phys_regs_s)"
puts $dump_file "========================================="

for {set i 0} {$i < 64} {incr i} {
    set reg_val [get_value /riscv_top_tb/TOP_RISCV_1/data_path_1/phys_regs_s($i)]
    puts $dump_file "reg_$i = $reg_val"
}

puts $dump_file ""
puts $dump_file "========================================="
puts $dump_file "COMMIT RENAME TABLE (commit_rename_table_s)"
puts $dump_file "========================================="
puts $dump_file "Arch reg -> Physical reg"
puts $dump_file "-----------------------------------------"

for {set i 0} {$i < 32} {incr i} {
    set phys_reg [get_value /riscv_top_tb/TOP_RISCV_1/data_path_1/u_renaming_module/commit_rename_table_s($i)]
    puts $dump_file "x$i       -> reg_$phys_reg"
}

puts $dump_file "========================================="

close $dump_file

# Cycle accounting up to the last commit
set names {cycles instructions rob_empty refill mispredicts jalr rob_full freelist lsq_full iq_full ex_busy}
set ipc_file [open "../tests/$::test/ipc.txt" w]
for {set i 0} {$i < [llength $names]} {incr i} {
    set v($i) [get_value -radix unsigned /riscv_top_tb/cnt_at_commit_s($i)]
    puts $ipc_file "[lindex $names $i] $v($i)"
}
puts $ipc_file "head_wait [expr {$v(0) - $v(1) - $v(2)}]"
puts $ipc_file "ipc [format %.3f [expr {$v(0) > 0 ? double($v(1)) / $v(0) : 0.0}]]"
close $ipc_file

puts "Dump completed!"

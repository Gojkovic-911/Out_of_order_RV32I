# Script directory
variable dispScriptFile [file normalize [info script]]
proc getScriptDirectory {} {
    variable dispScriptFile
    set scriptFolder [file dirname $dispScriptFile]
    return $scriptFolder
}

unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

set testname [lindex $argv 0]

cd [getScriptDirectory]

# Shared configuration
source ../../../RV32I/scripts/tcl/config.tcl
set results $cpu_sim_out
file mkdir $results
cd $results

set projectDir $results/RISCV_project

file mkdir $projectDir

# Create or open project
set project_name "RISCV_project"
set project_file "$projectDir/$project_name.xpr"

if { [file exists $project_file] } {
    puts "Project already exists. Opening existing project..."
    open_project $project_file
} else {
    puts "Project does not exist. Creating new project..."
    create_project $project_name $projectDir -part $part -force

    add_files -norecurse $src/rtl/riscv_top_mem.vhd 
    add_files -norecurse $src/rtl/riscv_top.vhd 
    add_files -norecurse $src/rtl/axi_lite_master.vhd
    add_files -norecurse $src/rtl/BRAM_byte_addressable.vhd
    add_files -norecurse $src/rtl/data_path/immediate.vhd 
    add_files -norecurse $src/rtl/data_path/ALU_simple.vhd 
    add_files -norecurse $src/rtl/data_path/LSU.vhd
    add_files -norecurse $src/rtl/data_path/LSQ.vhd
    add_files -norecurse $src/rtl/data_path/branch_decision_unit.vhd
    add_files -norecurse $src/rtl/data_path/free_list_fifo.vhd
    add_files -norecurse $src/rtl/data_path/renaming_module.vhd
    add_files -norecurse $src/rtl/data_path/issue_module.vhd
    add_files -norecurse $src/rtl/data_path/ex_router.vhd
    add_files -norecurse $src/rtl/data_path/alu_unit.vhd
    add_files -norecurse $src/rtl/data_path/branch_unit.vhd
    add_files -norecurse $src/rtl/data_path/cdb.vhd
    add_files -norecurse $src/rtl/data_path/ROB.vhd
    add_files -norecurse $src/rtl/data_path/data_path.vhd
    add_files -norecurse $src/rtl/control_path/control_path.vhd 
    add_files -norecurse $src/rtl/control_path/alu_decoder.vhd 
    add_files -norecurse $src/rtl/control_path/ctrl_decoder.vhd 
    add_files -norecurse $src/rtl/control_path/branch_predictor.vhd 
    add_files -norecurse $src/rtl/packages/alu_ops_pkg.vhd 
    add_files -norecurse $src/rtl/packages/instr_types_pkg.vhd
    add_files -norecurse $src/rtl/packages/txt_util.vhd
    add_files -fileset sim_1 -norecurse $tb/cpu/axi_lite_mem_model.vhd
    add_files -fileset sim_1 -norecurse $tb/cpu/riscv_top_tb.vhd
}

# CPU IP repository
set_property ip_repo_paths $out_dir/ip_repo [current_project]
update_ip_catalog
puts "IP catalog: [get_ipdefs -quiet -filter {NAME == CPU}]"

update_compile_order -fileset sources_1
set_property SOURCE_SET sources_1 [get_filesets sim_1]
update_compile_order -fileset sim_1

set_property top riscv_top_mem [get_filesets sources_1]
update_compile_order -fileset sources_1

# Testbench uses VHDL-2008 external names
set_property file_type {VHDL 2008} [get_files riscv_top_tb.vhd]
set_property top riscv_top_tb [get_filesets sim_1]
set_property generic [concat [cfg_generics CORE_GENERICS TB_GENERICS] PROGRAM_FILE=$prog_file DATA_FILE=$data_file] [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
update_compile_order -fileset sim_1

# Tests: TEST from argv, or all
source [getScriptDirectory]/tests.tcl
if {$testname ne "regression"} {
    set env(TEST) $testname
}
set tests [test_list $root]
puts "Found tests: $tests"

# Stage, run until idle fetch (at most MAX_CYCLES), check
set max_ns [expr {$cfg(MAX_CYCLES) * $cfg(TB_CLK_NS)}]
set first 1
foreach t $tests {
    set ::test $t
    puts [exec $scripts/prepare.sh $t cpu 2>@1]
    if {$first} {
        launch_simulation
        set first 0
    } else {
        restart
    }
    puts "Running test: $t"
    run $max_ns ns
    source [getScriptDirectory]/print_current_state.tcl
    puts [exec $scripts/check.sh $t $results 2>@1]
}

close_sim
exit

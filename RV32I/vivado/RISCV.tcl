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
source config.tcl

set projectDir ./RISCV_project

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

    add_files -norecurse ../rtl/riscv_top_mem.vhd 
    add_files -norecurse ../rtl/riscv_top.vhd 
    add_files -norecurse ../rtl/axi_lite_master.vhd
    add_files -norecurse ../rtl/BRAM_byte_addressable.vhd
    add_files -norecurse ../rtl/data_path/immediate.vhd 
    add_files -norecurse ../rtl/data_path/ALU_simple.vhd 
    add_files -norecurse ../rtl/data_path/register_bank.vhd 
    add_files -norecurse ../rtl/data_path/LSU.vhd
    add_files -norecurse ../rtl/data_path/LSQ.vhd
    add_files -norecurse ../rtl/data_path/branch_decision_unit.vhd
    add_files -norecurse ../rtl/data_path/free_list_fifo.vhd
    add_files -norecurse ../rtl/data_path/renaming_module.vhd
    add_files -norecurse ../rtl/data_path/issue_module.vhd
    add_files -norecurse ../rtl/data_path/ex_router.vhd
    add_files -norecurse ../rtl/data_path/alu_unit.vhd
    add_files -norecurse ../rtl/data_path/branch_unit.vhd
    add_files -norecurse ../rtl/data_path/cdb.vhd
    add_files -norecurse ../rtl/data_path/ROB.vhd
    add_files -norecurse ../rtl/data_path/data_path.vhd
    add_files -norecurse ../rtl/control_path/control_path.vhd 
    add_files -norecurse ../rtl/control_path/alu_decoder.vhd 
    add_files -norecurse ../rtl/control_path/ctrl_decoder.vhd 
    add_files -norecurse ../rtl/control_path/branch_predictor.vhd 
    add_files -norecurse ../rtl/packages/alu_ops_pkg.vhd 
    add_files -norecurse ../rtl/packages/instr_types_pkg.vhd
    add_files -norecurse ../rtl/packages/txt_util.vhd
    add_files -fileset sim_1 -norecurse ../tb/axi_lite_mem_model.vhd
    add_files -fileset sim_1 -norecurse ../tb/riscv_top_tb.vhd
}

# CPU IP repository
set_property ip_repo_paths [file normalize ../ip_repo] [current_project]
update_ip_catalog
puts "IP catalog: [get_ipdefs -quiet -filter {NAME == CPU}]"

update_compile_order -fileset sources_1
set_property SOURCE_SET sources_1 [get_filesets sim_1]
update_compile_order -fileset sim_1

set_property top riscv_top_mem [get_filesets sources_1]
update_compile_order -fileset sources_1

set_property top riscv_top_tb [get_filesets sim_1]
set_property generic [cfg_generics CORE_GENERICS SIM_GENERICS] [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
update_compile_order -fileset sim_1

# Find all tests
set tests {}
foreach d [glob -nocomplain -types d -directory ../tests *test] {
    lappend tests [file tail $d]
}

puts "Found tests: $tests"

if {$testname == "regression"} {

    # Compile first test
    set first_test [lindex $tests 0]
    puts "Compiling first test: $first_test"
    exec ../scripts/compile.sh $first_test
    
    launch_simulation
    
    foreach d $tests {
        set ::test $d

        puts "Assembling"
        exec ../scripts/compile.sh $d

        puts "Running test: $d"
        run 3000 us

        puts "Getting golden vector registers state"
        exec /usr/bin/python3 ../scripts/get_golden_vector.py $d
        puts "Printing current registers state"
        source print_current_state.tcl
        puts "Comparing "
        exec ../scripts/compare_regs.sh $d
        
        restart
    }
} else {
    set ::test $testname
    puts "Compiling test: $testname"
    exec ../scripts/compile.sh $testname
    
    launch_simulation
    puts "Running test: $testname"
    run 3000 us

    puts "Getting golden vector registers state"
    exec /usr/bin/python3 ../scripts/get_golden_vector.py $testname
    puts "Printing current registers state"
    source print_current_state.tcl
    puts "Comparing "
    exec ../scripts/compare_regs.sh $testname
}

close_sim
exit

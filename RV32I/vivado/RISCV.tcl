# Process for getting script file directory
variable dispScriptFile [file normalize [info script]]
proc getScriptDirectory {} {
    variable dispScriptFile
    set scriptFolder [file dirname $dispScriptFile]
    return $scriptFolder
}


unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

set testname [lindex $argv 0]

set num_threads 8

# Primjeni na sve faze
set_param general.maxThreads $num_threads
set_param synth.maxThreads $num_threads
set_param route.maxThreads $num_threads
set_param place.maxThreads $num_threads

# Change working directory to script file directory
cd [getScriptDirectory]

# Set project directory
set projectDir ./RISCV_project

file mkdir $projectDir

# MAKE A PROJECT
set project_name "RISCV_project"
set project_file "$projectDir/$project_name.xpr"

if { [file exists $project_file] } {
    puts "Project already exists. Opening existing project..."
    open_project $project_file
} else {
    puts "Project does not exist. Creating new project..."
    create_project $project_name $projectDir -force

    add_files -norecurse ../rtl/riscv_top_mem.vhd 
    add_files -norecurse ../rtl/TOP_RISCV.vhd 
    add_files -norecurse ../rtl/BRAM_byte_addressable.vhd
    add_files -norecurse ../rtl/data_path/immediate.vhd 
    add_files -norecurse ../rtl/data_path/ALU_simple.vhd 
    add_files -norecurse ../rtl/data_path/register_bank.vhd 
    add_files -norecurse ../rtl/data_path/LSU.vhd
    add_files -norecurse ../rtl/data_path/branch_decision_unit.vhd
    add_files -norecurse ../rtl/data_path/free_list_fifo.vhd
    add_files -norecurse ../rtl/data_path/renaming_module.vhd
    add_files -norecurse ../rtl/data_path/dispatch_module.vhd
    add_files -norecurse ../rtl/data_path/execute_unit.vhd
    add_files -norecurse ../rtl/data_path/ROB.vhd
    add_files -norecurse ../rtl/data_path/data_path.vhd
    add_files -norecurse ../rtl/control_path/control_path.vhd 
    add_files -norecurse ../rtl/control_path/alu_decoder.vhd 
    add_files -norecurse ../rtl/control_path/ctrl_decoder.vhd 
    add_files -norecurse ../rtl/control_path/branch_predictor.vhd 
    add_files -norecurse ../rtl/packages/alu_ops_pkg.vhd 
    add_files -norecurse ../rtl/packages/instr_types_pkg.vhd
    add_files -norecurse ../rtl/packages/txt_util.vhd
    add_files -fileset sim_1 -norecurse ../RISCV_tb/TOP_RISCV_tb.vhd
}

update_compile_order -fileset sources_1
set_property SOURCE_SET sources_1 [get_filesets sim_1]
update_compile_order -fileset sim_1

set_property top riscv_top_mem [get_filesets sources_1]
update_compile_order -fileset sources_1

set_property top TOP_RISCV_tb [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
update_compile_order -fileset sim_1

# Find all test directories (FIXED)
set tests {}
foreach d [glob -nocomplain -types d -directory ../RISCV_tb *test] {
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
        run 300 us

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
    run 300 us

    puts "Getting golden vector registers state"
    exec /usr/bin/python3 ../scripts/get_golden_vector.py $testname
    puts "Printing current registers state"
    source print_current_state.tcl
    puts "Comparing "
    exec ../scripts/compare_regs.sh $testname
}

close_sim
exit

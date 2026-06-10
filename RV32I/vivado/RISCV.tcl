#process for getting script file directory
variable dispScriptFile [file normalize [info script]]
proc getScriptDirectory {} {
    variable dispScriptFile
    set scriptFolder [file dirname $dispScriptFile]
    return $scriptFolder
}

#change working directory to script file directory
cd [getScriptDirectory]
#set project directory
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


# -part xc7z010clg400-1 
# set_property board_part digilentinc.com:zybo-z7-10:part0:1.0 [current_project]

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

launch_simulation
run 300 us

source ../scripts/print_current_state.tcl

close_sim

exit

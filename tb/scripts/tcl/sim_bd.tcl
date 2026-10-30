# Platform simulation project: builds the snapshot, then runs run_bd.tcl

set script_dir [file dirname [file normalize [info script]]]
source $script_dir/../../../RV32I/scripts/tcl/config.tcl
source $script_dir/tests.tcl
set results $bd_sim_out
file mkdir $results
cd $results
source $platform_dir/platform_bd.tcl

# Memory files must exist for the elaboration, run_bd.tcl loads the real ones
if {![file exists $prog_file]} { close [open $prog_file w] }
if {![file exists $data_file]} { close [open $data_file w] }

set prj_dir $results/sim
create_project sim $prj_dir -part $part -force
set_property ip_repo_paths $out_dir/ip_repo [current_project]
update_ip_catalog
create_platform_bd design_1 $prog_file $data_file true

set bd_file [get_files design_1.bd]
generate_target simulation $bd_file
add_files -norecurse [make_wrapper -files $bd_file -top]
add_files -fileset sim_1 -norecurse $tb/fpga/$platform/platform_tb.vhd
set_property file_type {VHDL 2008} [get_files platform_tb.vhd]
set_property top platform_tb [get_filesets sim_1]
set_property generic BOARD_CLK_MHZ=$board_clk_mhz [get_filesets sim_1]
update_compile_order -fileset sim_1

launch_simulation
source $script_dir/run_bd.tcl
close_sim
close_project

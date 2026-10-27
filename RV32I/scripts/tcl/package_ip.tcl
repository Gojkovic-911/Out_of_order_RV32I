# Package CPU as IP

set script_dir [file dirname [file normalize [info script]]]

source $script_dir/config.tcl
cd $out_dir
set rtl      $src/rtl
set ip_dir   $out_dir/ip_repo/CPU
set prj_dir  $out_dir/ip_package

create_project ip_package $prj_dir -part $part -force

# Core sources, VHDL-2008
set core_files [list \
    $rtl/packages/alu_ops_pkg.vhd \
    $rtl/packages/instr_types_pkg.vhd \
    $rtl/BRAM_byte_addressable.vhd \
    $rtl/control_path/ctrl_decoder.vhd \
    $rtl/control_path/alu_decoder.vhd \
    $rtl/control_path/branch_predictor.vhd \
    $rtl/control_path/control_path.vhd \
    $rtl/data_path/ALU_simple.vhd \
    $rtl/data_path/LSU.vhd \
    $rtl/data_path/LSQ.vhd \
    $rtl/data_path/branch_decision_unit.vhd \
    $rtl/data_path/immediate.vhd \
    $rtl/data_path/register_bank.vhd \
    $rtl/data_path/free_list_fifo.vhd \
    $rtl/data_path/renaming_module.vhd \
    $rtl/data_path/ROB.vhd \
    $rtl/data_path/issue_module.vhd \
    $rtl/data_path/ex_router.vhd \
    $rtl/data_path/alu_unit.vhd \
    $rtl/data_path/branch_unit.vhd \
    $rtl/data_path/cdb.vhd \
    $rtl/data_path/data_path.vhd \
    $rtl/riscv_top.vhd \
    $rtl/axi_lite_master.vhd]
set top_file $rtl/riscv_top_mem.vhd

add_files -norecurse $core_files
add_files -norecurse $top_file
set_property file_type {VHDL 2008} [get_files $core_files]
set_property file_type VHDL [get_files $top_file]
set_property top riscv_top_mem [current_fileset]
update_compile_order -fileset sources_1

# Package without copying sources
ipx::package_project -root_dir $ip_dir -vendor user.org -library user -taxonomy /UserIP -force

set core [ipx::current_core]
set_property name         CPU                          $core
set_property display_name "CPU"                        $core
set_property description  "RV32I out-of-order core, internal instruction memory, AXI4-Lite data master" $core
set_property version      2.0                          $core

ipx::create_xgui_files $core
ipx::update_checksums  $core
ipx::check_integrity   $core
ipx::save_core         $core

# Refresh IP catalog
set_property ip_repo_paths [file dirname $ip_dir] [current_project]
update_ip_catalog

# Report inferred interfaces
foreach bi [ipx::get_bus_interfaces -of_objects $core] {
    puts "BUS INTERFACE: [get_property name $bi]  [get_property abstraction_type_vlnv $bi]  mode=[get_property interface_mode $bi]"
    foreach p [ipx::get_bus_parameters -of_objects $bi] {
        puts "    [get_property name $p] = [get_property value $p]"
    }
}

close_project

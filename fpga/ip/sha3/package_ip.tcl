# Package SHA3 accelerator as IP

set script_dir [file dirname [file normalize [info script]]]
source $script_dir/../../../RV32I/scripts/tcl/config.tcl
set ip_dir  $fpga_ip_repo/sha3
set prj_dir $root/out/fpga/ip_package/sha3
file mkdir $prj_dir
cd $prj_dir

create_project ip_package $prj_dir -part $part -force

# Sources VHDL-2008, top plain VHDL (packager limit)
set files [concat [glob $script_dir/*.vhd] $root/fpga/rtl/axi_lite_regs.vhd]
add_files -norecurse $files
set_property file_type {VHDL 2008} [get_files $files]
set_property file_type VHDL [get_files $script_dir/sha3.vhd]
set_property top sha3 [current_fileset]
update_compile_order -fileset sources_1

# Package without copying sources
ipx::package_project -root_dir $ip_dir -vendor user.org -library user -taxonomy /UserIP -force

set core [ipx::current_core]
set_property name         SHA3                         $core
set_property display_name "SHA3"                       $core
set_property description  "SHA3-256, AXI4-Lite control, 64-bit AXI-Stream in/out, polled status" $core
set_property version      1.0                          $core

# One clock and reset for the register and both stream interfaces
foreach busif {s_axi s_axis m_axis} {
    ipx::associate_bus_interfaces -busif $busif -clock aclk $core
}

ipx::create_xgui_files $core
ipx::update_checksums  $core
ipx::check_integrity   $core
ipx::save_core         $core

# Report inferred interfaces
foreach bi [ipx::get_bus_interfaces -of_objects $core] {
    puts "BUS INTERFACE: [get_property name $bi]  [get_property abstraction_type_vlnv $bi]  mode=[get_property interface_mode $bi]"
    foreach p [ipx::get_bus_parameters -of_objects $bi] {
        puts "    [get_property name $p] = [get_property value $p]"
    }
}

close_project

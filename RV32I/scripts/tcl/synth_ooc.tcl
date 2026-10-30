# CPU out-of-context synthesis

set script_dir [file dirname [file normalize [info script]]]

source $script_dir/config.tcl
cd $out_dir
set rtl     $src/rtl
set out     $out_dir/synth
set prog    [file normalize [lindex $argv 0]]
set period  [expr {[llength $argv] > 1 ? [lindex $argv 1] : 1000.0 / $cfg(SYNTH_CLK_MHZ)}]
file mkdir $out

read_vhdl -vhdl2008 [list \
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
read_vhdl $rtl/riscv_top_mem.vhd

# Clock constraint
set xdc $out/clock.xdc
set fh [open $xdc w]
puts $fh "create_clock -name clk -period $period \[get_ports clk\]"
close $fh
read_xdc -mode out_of_context $xdc

# Generics from cpu.mk
set generics {}
foreach g [cfg_generics CORE_GENERICS] {
    lappend generics -generic $g
}
foreach name $cfg(SYNTH_GENERICS) {
    lappend generics -generic "$name=\"$cfg($name)\""
}
synth_design -top riscv_top_mem -part $cfg(SYNTH_PART) -mode out_of_context {*}$generics -generic "INIT_FILE=\"$prog\"" {*}[effort_args synth]
report_utilization                -file $out/utilization_synth.rpt
report_utilization -hierarchical  -file $out/utilization_synth_hier.rpt
report_ram_utilization            -file $out/ram_synth.rpt

opt_design   {*}[effort_args opt]
place_design {*}[effort_args place]
if {[effort_args phys_opt] ne ""} {
    phys_opt_design {*}[effort_args phys_opt]
}
route_design {*}[effort_args route]
if {[effort_args phys_opt] ne ""} {
    phys_opt_design {*}[effort_args phys_opt]
}
report_utilization                -file $out/utilization_impl.rpt
report_utilization -hierarchical  -file $out/utilization_impl_hier.rpt
report_timing_summary -max_paths 10 -file $out/timing_impl.rpt
write_paths $out/paths.txt
report_power -file $out/power.rpt
write_checkpoint -force $out/riscv_top_mem_routed.dcp

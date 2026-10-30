# Implementation only, from the last platform synthesis

set script_dir [file dirname [file normalize [info script]]]
source $script_dir/../../RV32I/scripts/tcl/config.tcl
cd $fpga_out

set prj_dir $fpga_out/project
set out     $fpga_out/impl/$cfg(PLACE_DIRECTIVE)_$cfg(ROUTE_DIRECTIVE)
if {![file exists $prj_dir/project.xpr]} {
    error "No platform project in $prj_dir, run make bitstream first"
}

# Same steps as impl_1 in bitstream.tcl
open_project $prj_dir/project.xpr
open_run synth_1 -name synth_1
opt_design      {*}[effort_args opt]
place_design    {*}[effort_args place]
phys_opt_design {*}[effort_args phys_opt]
route_design    {*}[effort_args route]
phys_opt_design {*}[effort_args phys_opt]

file mkdir $out
report_utilization    -file $out/utilization.rpt
report_timing_summary -file $out/timing.rpt
write_paths $out/paths.txt
report_power -file $out/power.rpt
close_project

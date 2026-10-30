# FPGA bitstream build

set script_dir [file dirname [file normalize [info script]]]
source $script_dir/../../RV32I/scripts/tcl/config.tcl
file mkdir $fpga_out
cd $fpga_out
source $platform_dir/platform_bd.tcl
source $script_dir/mem_mmi.tcl

set prog    [file normalize [lindex $argv 0]]
set data    [file normalize [lindex $argv 1]]
set prj_dir $fpga_out/project
set out     $fpga_out/bitstream
set bd_name design_1

if {![file exists $prog]} {
    error "Program file not found: $prog"
}

create_project project $prj_dir -part $part -force
set_property ip_repo_paths $out_dir/ip_repo [current_project]
update_ip_catalog

create_platform_bd $bd_name $prog $data false

set bd_file [get_files $bd_name.bd]
generate_target all $bd_file
add_files -norecurse [make_wrapper -files $bd_file -top]
set_property top ${bd_name}_wrapper [current_fileset]
add_files -fileset constrs_1 -norecurse [glob $board_dir/*.xdc]
update_compile_order -fileset sources_1

# Run directives for IMPL_EFFORT
if {$cfg(IMPL_EFFORT) eq "performance"} {
    create_ip_run $bd_file
    foreach r [get_runs -filter {IS_SYNTHESIS}] {
        set_property STEPS.SYNTH_DESIGN.ARGS.DIRECTIVE $cfg(SYNTH_DIRECTIVE) $r
        set_property STEPS.SYNTH_DESIGN.ARGS.GLOBAL_RETIMING on             $r
    }
    set impl [get_runs impl_1]
    set_property STEPS.OPT_DESIGN.ARGS.DIRECTIVE                $cfg(OPT_DIRECTIVE) $impl
    set_property STEPS.PLACE_DESIGN.ARGS.DIRECTIVE              $cfg(PLACE_DIRECTIVE) $impl
    set_property STEPS.PHYS_OPT_DESIGN.IS_ENABLED               true              $impl
    set_property STEPS.PHYS_OPT_DESIGN.ARGS.DIRECTIVE           $cfg(PHYS_OPT_DIRECTIVE) $impl
    set_property STEPS.ROUTE_DESIGN.ARGS.DIRECTIVE              $cfg(ROUTE_DIRECTIVE) $impl
    set_property STEPS.POST_ROUTE_PHYS_OPT_DESIGN.IS_ENABLED    true              $impl
    set_property STEPS.POST_ROUTE_PHYS_OPT_DESIGN.ARGS.DIRECTIVE $cfg(PHYS_OPT_DIRECTIVE) $impl
}

launch_runs impl_1 -to_step write_bitstream -jobs $max_jobs
wait_on_run impl_1
if {[get_property PROGRESS [get_runs impl_1]] != "100%"} {
    error "Implementation failed, see $prj_dir/project.runs/impl_1"
}

file mkdir $out
open_run impl_1
report_utilization    -file $out/utilization.rpt
report_timing_summary -file $out/timing.rpt
write_paths $out/paths.txt
report_power -file $out/power.rpt
write_mem_mmi $out/mem.mmi $part [list \
    imem *INSTR_MEM* 0          [expr {1 << $cfg(WADDR)}] \
    dmem *dmem_0*    $dmem_base [expr {1 << [mem_bits $dmem_size]}]]
file copy -force [glob $prj_dir/project.runs/impl_1/*.bit] $out/
puts "Bitstream: [glob $out/*.bit]"

close_project

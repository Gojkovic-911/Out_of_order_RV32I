# Regression on the CPU platform, or one firmware run for the waveform

set script_dir [file dirname [file normalize [info script]]]
source $script_dir/../../../RV32I/scripts/tcl/config.tcl
source $script_dir/tests.tcl
set results $bd_sim_out
file mkdir $results
cd $results
source $platform_dir/platform_bd.tcl

unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

set prj_dir $results/sim
set max_us  $cfg(BD_MAX_US)
set fw_dir  [expr {[info exists env(FW_DIR)] ? $env(FW_DIR) : ""}]

# Run until fetch stays at zero
proc run_program {max_us idle_us} {
    set fetch /platform_tb/dut/design_1_i/CPU_0/U0/instr_rdata
    set started 0
    set idle 0
    for {set t 0} {$t < $max_us && $idle < $idle_us} {incr t} {
        run 1 us
        if {[get_value -radix unsigned $fetch] != 0} {
            set started 1
            set idle 0
        } elseif {$started} {
            incr idle
        }
    }
}

# Program for the first launch: firmware or the first test
if {$fw_dir ne ""} {
    set prog $fw_dir/instr_mem.txt
    set data $fw_dir/data_mem.txt
} else {
    set tests [test_list $root]
    puts [exec $scripts/prepare.sh [lindex $tests 0] $platform 2>@1]
    set prog $prog_file
    set data $data_file
}

create_project sim $prj_dir -part $part -force
set_property ip_repo_paths $out_dir/ip_repo [current_project]
update_ip_catalog
create_platform_bd design_1 $prog $data true

set bd_file [get_files design_1.bd]
generate_target simulation $bd_file
add_files -norecurse [make_wrapper -files $bd_file -top]
add_files -fileset sim_1 -norecurse $tb/fpga/$platform/platform_tb.vhd
set_property file_type {VHDL 2008} [get_files platform_tb.vhd]
set_property top platform_tb [get_filesets sim_1]
set_property generic "BOARD_CLK_MHZ=$board_clk_mhz RESET_US=$cfg(RESET_US)" [get_filesets sim_1]
update_compile_order -fileset sim_1

launch_simulation

# Firmware: fixed run time, all signals in the waveform database
if {$fw_dir ne ""} {
    set sim_us $cfg(FW_SIM_US)
    restart
    log_wave -recursive /
    run $sim_us us
    puts "Waveform: [glob -nocomplain $prj_dir/sim.sim/sim_1/behav/xsim/*.wdb]"
    close_sim
    close_project
    return
}

set dp ""
foreach s [get_scopes -r /platform_tb/*] {
    if {[string match */data_path_1 $s]} { set dp $s; break }
}
puts "Data path scope: $dp"

# Program swap, restart (memories reload their files), check
set first 1
foreach t $tests {
    set ::test $t
    set ::dp   $dp
    if {!$first} {
        puts [exec $scripts/prepare.sh $t $platform 2>@1]
        restart
    }
    set first 0
    run_program $max_us $cfg(BD_IDLE_US)
    source $scripts/tcl/print_current_state.tcl
    puts [exec $scripts/check.sh $t $results 2>@1]
}

close_sim
close_project

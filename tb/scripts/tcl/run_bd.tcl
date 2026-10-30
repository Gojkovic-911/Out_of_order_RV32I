# Platform regression or firmware run on a launched simulation (Vivado or plain xsim)

unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

set root     [file normalize [file join [file dirname [info script]] ../../..]]
set scripts  $root/tb/scripts
set stage    $root/out/tb/tests
set platform $env(PLATFORM)
set results  $root/out/tb/fpga/$env(PLATFORM)_$env(BOARD)
set dump     $scripts/tcl/print_current_state.tcl
set fw_dir   [expr {[info exists env(FW_DIR)] ? $env(FW_DIR) : ""}]
source $scripts/tcl/tests.tcl

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

# Firmware: fixed run time, all signals in the waveform database
if {$fw_dir ne ""} {
    file copy -force $fw_dir/instr_mem.txt $fw_dir/data_mem.txt $stage
    restart
    log_wave -recursive /
    run 200 us
    puts "Waveform: [glob -nocomplain $results/sim/sim.sim/sim_1/behav/xsim/*.wdb]"
} else {
    set dp ""
    foreach s [get_scopes -r /platform_tb/*] {
        if {[string match */data_path_1 $s]} { set dp $s; break }
    }
    puts "Data path scope: $dp"

    # Program swap, restart (memories reload their files), check
    foreach t [test_list $root] {
        set ::test $t
        set ::dp   $dp
        puts [exec $scripts/prepare.sh $t $platform 2>@1]
        restart
        # At most 2 ms, ends after 5 us of zero fetch
        run_program 2000 5
        source $dump
        puts [exec $scripts/check.sh $t $results 2>@1]
    }
}

# Plain xsim run ends here, Vivado closes the simulation itself
if {[info commands close_project] eq ""} { quit }

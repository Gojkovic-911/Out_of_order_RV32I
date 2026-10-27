# Regression on the Zybo block design

set script_dir [file dirname [file normalize [info script]]]
cd $script_dir
source $script_dir/config.tcl
source $script_dir/platform_bd.tcl

unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

set prog    [file normalize ../tests/assembly_code_bin.txt]
set prj_dir [file normalize ./zybo_sim]
set max_us  [expr {[info exists env(BD_RUN_US)] ? $env(BD_RUN_US) : 400}]

# Run until fetch stays at zero
proc run_program {max_us} {
    set fetch /zybo_tb/dut/design_1_i/CPU_0/U0/instr_rdata
    set started 0
    set idle 0
    for {set t 0} {$t < $max_us && $idle < 5} {incr t} {
        run 1 us
        if {[get_value -radix unsigned $fetch] != 0} {
            set started 1
            set idle 0
        } elseif {$started} {
            incr idle
        }
    }
}

set tests {}
if {[info exists env(TEST)] && $env(TEST) ne ""} {
    set tests [list $env(TEST)]
} else {
    foreach d [lsort [glob -nocomplain -types d -directory ../tests *test]] {
        lappend tests [file tail $d]
    }
}
exec ../scripts/compile.sh [lindex $tests 0]

create_project zybo_sim $prj_dir -part $part -force
set_property ip_repo_paths [file normalize ../ip_repo] [current_project]
update_ip_catalog
create_platform_bd design_1 $prog

set bd_file [get_files design_1.bd]
generate_target simulation $bd_file
add_files -norecurse [make_wrapper -files $bd_file -top]
add_files -fileset sim_1 -norecurse ../tb/zybo_tb.vhd
set_property file_type {VHDL 2008} [get_files zybo_tb.vhd]
set_property top zybo_tb [get_filesets sim_1]
# Full elaboration, so INIT_FILE is read again
set_property INCREMENTAL false [get_filesets sim_1]
update_compile_order -fileset sim_1

# Program swap, relaunch, compare
launch_simulation
set dp ""
foreach s [get_scopes -r /zybo_tb/*] {
    if {[string match */data_path_1 $s]} { set dp $s; break }
}
puts "Data path scope: $dp"

foreach t $tests {
    set ::test $t
    set ::dp   $dp
    exec ../scripts/compile.sh $t
    relaunch_sim
    run_program $max_us
    exec /usr/bin/python3 ../scripts/get_golden_vector.py $t
    source print_current_state.tcl
    exec ../scripts/compare_regs.sh $t
}

close_sim
close_project

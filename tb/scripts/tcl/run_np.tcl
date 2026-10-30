# Non-project regression in xsim

unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

set root    [file normalize [file join [file dirname [info script]] ../../..]]
set scripts $root/tb/scripts
set results $root/out/tb/cpu
set dump    [file dirname [file normalize [info script]]]/print_current_state.tcl
source [file dirname [file normalize [info script]]]/tests.tcl

set tests [test_list $root]
puts "Found tests: $tests"

# Stage, run until idle fetch (at most MAX_CYCLES), check
set max_ns [expr {$env(CFG_MAX_CYCLES) * $env(CFG_TB_CLK_NS)}]
foreach t $tests {
    set ::test $t
    puts [exec $scripts/prepare.sh $t cpu 2>@1]
    restart
    puts "Running test: $t"
    run $max_ns ns
    source $dump
    puts [exec $scripts/check.sh $t $results 2>@1]
}

quit

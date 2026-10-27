# Non-project regression in xsim

unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

set root      [file normalize [file join [file dirname [info script]] ../../..]]
set scripts   $root/RV32I/scripts
set tests_src $root/RV32I/tb/tests
set tests_out $root/out/tests
set dump      [file dirname [file normalize [info script]]]/print_current_state.tcl

set tests {}
if {[info exists env(TEST)] && $env(TEST) ne ""} {
    set tests [list $env(TEST)]
} else {
    foreach d [lsort [glob -nocomplain -types d -directory $tests_src *test]] {
        lappend tests [file tail $d]
    }
}
puts "Found tests: $tests"

foreach d $tests {
    set ::test $d

    puts "Assembling"
    exec $scripts/compile.sh $d

    restart
    puts "Running test: $d"
    run 3000 us

    puts "Getting golden vector registers state"
    exec /usr/bin/python3 $scripts/get_golden_vector.py $d
    puts "Printing current registers state"
    source $dump
    puts "Comparing "
    exec $scripts/compare_regs.sh $d
}

quit

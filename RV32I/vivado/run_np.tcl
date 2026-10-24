# Non-project regression (make np)
# Runs inside xsim on the snapshot built by the Makefile, the program of each
# test is swapped with compile.sh + restart (the testbench reads it at time 0).
# Same helper scripts as RISCV.tcl, run from this directory.

unset -nocomplain env(PYTHONHOME)
unset -nocomplain env(PYTHONPATH)

cd [file dirname [file normalize [info script]]]

set tests {}
foreach d [lsort [glob -nocomplain -types d -directory ../RISCV_tb *test]] {
    lappend tests [file tail $d]
}
puts "Found tests: $tests"

foreach d $tests {
    set ::test $d

    puts "Assembling"
    exec ../scripts/compile.sh $d

    restart
    puts "Running test: $d"
    run 3000 us

    puts "Getting golden vector registers state"
    exec /usr/bin/python3 ../scripts/get_golden_vector.py $d
    puts "Printing current registers state"
    source print_current_state.tcl
    puts "Comparing "
    exec ../scripts/compare_regs.sh $d
}

quit

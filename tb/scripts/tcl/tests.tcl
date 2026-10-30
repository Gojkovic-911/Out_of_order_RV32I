# Test list, plain Tcl (also sourced by xsim)

# TEST from env, or every tb/tests/<suite>/<name>/ with <name>.s or .c files
proc test_list {root} {
    if {[info exists ::env(TEST)] && $::env(TEST) ne ""} {
        return [list $::env(TEST)]
    }
    set tests {}
    foreach d [lsort [glob -nocomplain -types d $root/tb/tests/*/*]] {
        set n [file tail $d]
        if {[file exists $d/$n.s] || [llength [glob -nocomplain $d/*.c]] > 0} {
            lappend tests $n
        }
    }
    return $tests
}

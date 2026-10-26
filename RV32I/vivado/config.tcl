# Load config.mk into cfg

array set cfg {}
set cfg_file [file normalize [file join [file dirname [info script]] ../config.mk]]
if {[info exists env(CONFIG_MK)]} {
    set cfg_file $env(CONFIG_MK)
}
set fh [open $cfg_file]
foreach line [split [read $fh] "\n"] {
    if {[regexp {^\s*(\w+)\s*=\s*(.*?)\s*$} $line -> name value]} {
        set cfg($name) $value
    }
}
close $fh

set part          $cfg(PART)
set max_jobs      $cfg(MAX_JOBS)
set board_clk_mhz $cfg(BOARD_CLK_MHZ)
set cpu_clk_mhz   $cfg(CPU_CLK_MHZ)
set dmem_base     $cfg(DMEM_BASE)
set dmem_size     $cfg(DMEM_SIZE)
set gpio_base     $cfg(GPIO_BASE)

set_param general.maxThreads $max_jobs

# Step options for IMPL_EFFORT
proc effort_args {step} {
    global cfg
    if {$cfg(IMPL_EFFORT) ne "performance"} {
        return {}
    }
    return [dict get {
        synth    {-directive PerformanceOptimized -retiming}
        opt      {-directive Explore}
        place    {-directive ExtraTimingOpt}
        phys_opt {-directive AggressiveExplore}
        route    {-directive AggressiveExplore}
    } $step]
}

# Worst setup paths, one per endpoint
proc write_paths {file} {
    set fh [open $file w]
    foreach p [get_timing_paths -max_paths 2000 -nworst 1 -setup] {
        puts $fh [join [list [get_property SLACK $p] [get_property LOGIC_LEVELS $p] \
            [get_property DATAPATH_DELAY $p] [get_property STARTPOINT_PIN $p] \
            [get_property ENDPOINT_PIN $p]] |]
    }
    close $fh
}

# Generic NAME=value pairs
proc cfg_generics {args} {
    global cfg
    set pairs {}
    foreach group $args {
        foreach name $cfg($group) {
            lappend pairs "$name=$cfg($name)"
        }
    }
    return $pairs
}

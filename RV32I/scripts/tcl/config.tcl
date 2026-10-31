# Source and output paths
set root      [file normalize [file join [file dirname [info script]] ../../..]]
set src       $root/RV32I
set tb        $root/tb
set scripts   $tb/scripts
set tests_src $tb/tests/stress_tests
set out_dir   $root/out/RV32I
set tests_out $root/out/tb/tests
set cpu_sim_out $root/out/tb/cpu
set prog_file $tests_out/instr_mem.txt
set data_file $tests_out/data_mem.txt
file mkdir $out_dir $tests_out

# Platform and board selection
set platform  [expr {[info exists env(PLATFORM)] ? $env(PLATFORM) : "simple"}]
set board     [expr {[info exists env(BOARD)] ? $env(BOARD) : "zybo"}]
set platform_dir $root/fpga/platforms/$platform
set board_dir    $root/fpga/boards/$board
set fpga_out     $root/out/fpga/${platform}_$board
set bd_sim_out   $root/out/tb/fpga/${platform}_$board

# Load cpu.mk, <platform>.mk, board.mk, tools.mk, tb.mk into cfg; make exports command line overrides
array set cfg {}
set cfg_files [list \
    [expr {[info exists env(CONFIG_MK)] ? $env(CONFIG_MK) : "$src/cpu.mk"}] \
    $platform_dir/$platform.mk \
    $board_dir/board.mk \
    $root/tools/tools.mk \
    $tb/tb.mk]
foreach cfg_file $cfg_files {
    set fh [open $cfg_file]
    foreach line [split [read $fh] "\n"] {
        if {[regexp {^\s*(\w+)\s*=\s*(.*?)\s*$} $line -> name value]} {
            set cfg($name) [expr {[info exists env(CFG_$name)] ? $env(CFG_$name) : $value}]
        }
    }
    close $fh
}

# CPU synthesis target: own value, or the selected platform / board
if {$cfg(SYNTH_CLK_MHZ) eq "platform"} { set cfg(SYNTH_CLK_MHZ) $cfg(CPU_CLK_MHZ) }
if {$cfg(SYNTH_PART) eq "board"}       { set cfg(SYNTH_PART) $cfg(PART) }

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
    return [dict get [list \
        synth    [list -directive $cfg(SYNTH_DIRECTIVE) -retiming] \
        opt      [list -directive $cfg(OPT_DIRECTIVE)] \
        place    [list -directive $cfg(PLACE_DIRECTIVE)] \
        phys_opt [list -directive $cfg(PHYS_OPT_DIRECTIVE)] \
        route    [list -directive $cfg(ROUTE_DIRECTIVE)] \
    ] $step]
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

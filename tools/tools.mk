# Machine and tool settings

# Vivado binary: path, auto (found with locate)
VIVADO             = auto

# RISC-V toolchain prefix
CROSS              = riscv64-unknown-elf

# Python with playwright: path (golden vectors)
PYTHON             = /usr/bin/python3

# C flags for the CPU
# -mstrict-align: RV32I has no misaligned accesses
CFLAGS             = -O2 -mstrict-align

# Parallel Vivado runs and threads: number
# more exhausts the RAM (3 for a 15 GB machine)
MAX_JOBS           = 3

# Directive set: default (Vivado defaults), performance (directives below)
# performance also adds retiming and phys_opt; better WNS, longer runs
IMPL_EFFORT        = performance

# Synthesis directive: Default, PerformanceOptimized, AreaOptimized_high, ...
SYNTH_DIRECTIVE    = PerformanceOptimized

# Opt directive: Default, Explore, ExploreArea, ...
OPT_DIRECTIVE      = Explore

# Place directive: Default, Explore, ExtraNetDelay_high, AltSpreadLogic_high, ...
# sweep.py --directives compares them
PLACE_DIRECTIVE    = ExtraNetDelay_high

# Phys_opt directive: Default, Explore, AggressiveExplore, ...
PHYS_OPT_DIRECTIVE = AggressiveExplore

# Route directive: Default, Explore, AggressiveExplore, ...
ROUTE_DIRECTIVE    = AggressiveExplore

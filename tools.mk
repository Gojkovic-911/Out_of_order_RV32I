# Machine and tool settings
# Format: NAME = value (comments on their own line, never after a value).

# Vivado binary, auto: found with locate
VIVADO             = auto
# RISC-V toolchain prefix
CROSS              = riscv64-unknown-elf
# Python with playwright (golden vectors)
PYTHON             = /usr/bin/python3
# C flags for the CPU: no misaligned accesses
CFLAGS             = -O2 -mstrict-align

# Parallel Vivado runs and threads: more exhausts the RAM (15 GB machine)
MAX_JOBS           = 3

# Timing effort: default or performance (directives below, retiming, phys_opt)
IMPL_EFFORT        = performance
SYNTH_DIRECTIVE    = PerformanceOptimized
OPT_DIRECTIVE      = Explore
PLACE_DIRECTIVE    = ExtraNetDelay_high
PHYS_OPT_DIRECTIVE = AggressiveExplore
ROUTE_DIRECTIVE    = AggressiveExplore

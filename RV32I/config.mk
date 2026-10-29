# CPU and platform configuration, one place for every tool.
# Read by the Makefile (include) and by the Vivado scripts (scripts/tcl/config.tcl).
# Format: NAME = value (comments on their own line, never after a value).
# Only the defaults are verified by the regression.

# ---- Core: generics of riscv_top_mem (IP, synthesis) and riscv_top_tb ----
CORE_GENERICS  = DATA_WIDTH MEM_BYTES ARCH_ADDR_BITS NUM_PHYS_REGS ROB_DEPTH IQ_DEPTH LSQ_DEPTH WADDR

# Data width in bits and bytes
DATA_WIDTH     = 32
MEM_BYTES      = 4
# Architectural register address bits (32 registers)
ARCH_ADDR_BITS = 5
# Physical registers (rename)
NUM_PHYS_REGS  = 64
# Reorder buffer entries
ROB_DEPTH      = 64
# Instruction queue entries
IQ_DEPTH       = 16
# Load/store queue entries
LSQ_DEPTH      = 16
# Instruction memory of 2^WADDR bytes (the testbench data memory too)
WADDR          = 14

# ---- Synthesis only: generics of riscv_top_mem (IP, synthesis) ----
SYNTH_GENERICS = RF_RAM_STYLE IQ_RAM_STYLE

# Vivado ram_style of the physical register file and the IQ payload:
# auto, distributed (LUTRAM), block (BRAM) or registers (flip-flops)
RF_RAM_STYLE   = auto
IQ_RAM_STYLE   = distributed

# ---- Simulation only: generics of riscv_top_tb ----
SIM_GENERICS      = DATA_MEM_MIN_WAIT DATA_MEM_MAX_WAIT

# AXI data memory model: MIN..MAX random wait cycles per handshake
DATA_MEM_MIN_WAIT = 0
DATA_MEM_MAX_WAIT = 3

# ---- FPGA: Zybo (original, Zynq-7010) ----
PART          = xc7z010clg400-1

# Parallel Vivado runs and threads: more exhausts the RAM (15 GB machine)
MAX_JOBS      = 3

# Timing effort of synthesis and implementation: default or performance
# (timing directives, retiming, phys_opt; same area, longer build)
IMPL_EFFORT   = performance

# Board oscillator in, CPU and bus clock out of clk_wiz (MHz)
BOARD_CLK_MHZ = 125
CPU_CLK_MHZ   = 120

# Address map of the CPU data port (firmware uses these addresses)
DMEM_BASE     = 0x00000000
DMEM_SIZE     = 16K
GPIO_BASE     = 0x40000000

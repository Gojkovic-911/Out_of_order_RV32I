# CPU configuration
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
ROB_DEPTH      = 32
# Instruction queue entries
IQ_DEPTH       = 8
# Load/store queue entries
LSQ_DEPTH      = 8
# Instruction memory of 2^WADDR bytes
WADDR          = 14

# ---- Synthesis only: generics of riscv_top_mem (IP, synthesis) ----
SYNTH_GENERICS = RF_RAM_STYLE IQ_RAM_STYLE

# Vivado ram_style of the physical register file and the IQ payload:
# auto, distributed (LUTRAM), block (BRAM) or registers (flip-flops)
RF_RAM_STYLE   = auto
IQ_RAM_STYLE   = distributed

# ---- CPU out-of-context synthesis (make cpu synth) ----
# Target clock (MHz), or platform: CPU_CLK_MHZ of PLATFORM
SYNTH_CLK_MHZ  = 120
# FPGA part, or board: PART of BOARD
SYNTH_PART     = xc7z010clg400-1

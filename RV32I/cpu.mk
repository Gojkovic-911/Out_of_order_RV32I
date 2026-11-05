# CPU configuration

# Generics of riscv_top_mem and riscv_top_tb
CORE_GENERICS   = DATA_WIDTH ARCH_ADDR_BITS NUM_PHYS_REGS ROB_DEPTH IQ_DEPTH LSQ_DEPTH IMEM_ADDR_BITS DMEM_BASE DMEM_ADDR_BITS DMEM_RD_LATENCY

# Data width: 32
DATA_WIDTH      = 32

# Architectural register bits: 5 (32 registers)
ARCH_ADDR_BITS  = 5

# Physical registers: power of two, above 32
# more: fewer free-list stalls, larger register file and rename tables
NUM_PHYS_REGS   = 64

# ROB entries: power of two
# above 32 rarely fills with 64 physical registers, more LUTs
ROB_DEPTH       = 32

# IQ entries: power of two
# 16 has no timing margin at 120 MHz (IQ full is the critical path)
IQ_DEPTH        = 8

# LSQ entries: power of two
LSQ_DEPTH       = 8

# IMEM size: 2^bits bytes
# bitstream MMI expects 14 (four byte-lane RAMB36)
IMEM_ADDR_BITS  = 14

# DMEM base: hex, below 0x80000000, aligned to the size
# loads/stores outside DMEM go to m_axi
DMEM_BASE       = 0x00000000

# DMEM size: 2^bits bytes
# bitstream MMI expects 14 (four byte-lane RAMB36)
DMEM_ADDR_BITS  = 14

# DMEM read latency: cycles, 1 or more
# 2: BRAM output register, ~2 ns shorter path; more: fabric registers; every cycle makes each load longer
DMEM_RD_LATENCY = 1

# Synthesis-only generics of riscv_top_mem
SYNTH_GENERICS  = RF_RAM_STYLE IQ_RAM_STYLE

# Register file RAM: auto, distributed, block, registers
# auto gives two RAMB18 here, distributed ~250 LUT more, same timing
RF_RAM_STYLE    = auto

# IQ payload RAM: auto, distributed, block, registers
IQ_RAM_STYLE    = distributed

# cpu synth clock: MHz, platform (CPU_CLK_MHZ of PLATFORM)
SYNTH_CLK_MHZ   = 120

# cpu synth part: part name, board (PART of BOARD)
SYNTH_PART      = xc7z010clg400-1

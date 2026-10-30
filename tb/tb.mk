# Testbench configuration
# Format: NAME = value (comments on their own line, never after a value).

# ---- CPU testbench: generics of riscv_top_tb ----
TB_GENERICS       = TB_CLK_NS IDLE_STOP_CYCLES DATA_MEM_BITS DATA_MEM_MIN_WAIT DATA_MEM_MAX_WAIT

# Clock period (ns)
TB_CLK_NS         = 100
# Fetched zero words that end a test
IDLE_STOP_CYCLES  = 200
# Upper limit of one test (cycles)
MAX_CYCLES        = 1000000
# AXI data memory model: 2^DATA_MEM_BITS bytes
DATA_MEM_BITS     = 14
# AXI data memory model: MIN..MAX random wait cycles per handshake
DATA_MEM_MIN_WAIT = 0
DATA_MEM_MAX_WAIT = 3

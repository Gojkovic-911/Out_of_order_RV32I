# Testbench configuration

# Generics of riscv_top_tb
TB_GENERICS       = TB_CLK_NS IDLE_STOP_CYCLES DATA_MEM_MIN_WAIT DATA_MEM_MAX_WAIT

# Testbench clock period: ns
TB_CLK_NS         = 100

# Test end: cycles of fetched zero words
IDLE_STOP_CYCLES  = 200

# Test limit: cycles
MAX_CYCLES        = 1000000

# DMEM wait: MIN..MAX random cycles per access, simulation only
# 0..0: fixed latency; longer waits stress the LSQ, lower the IPC
DATA_MEM_MIN_WAIT = 0
DATA_MEM_MAX_WAIT = 3

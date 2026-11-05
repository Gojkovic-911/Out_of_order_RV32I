# SHA3 platform: CPU (with its memories), GPIO, SHA3 accelerator fed by AXI DMA, clock wizard, reset

# CPU clock from clk_wiz: MHz
CPU_CLK_MHZ   = 120

# GPIO address on m_axi_lite
GPIO_BASE     = 0x40000000

# GPIO outputs: 1 to 4 (LD0-LD3)
GPIO_WIDTH    = 4

# SHA3 register address on m_axi_lite
SHA3_BASE     = 0x40010000

# AXI DMA register address on m_axi_lite
DMA_BASE      = 0x40020000

# Our fpga IPs: names in fpga/ip/, empty for none
FPGA_IPS      = sha3

# Firmware: in sw/fw/sha3/
DEFAULT_FW    = sha3

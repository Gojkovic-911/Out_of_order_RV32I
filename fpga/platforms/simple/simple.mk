# Simple platform: CPU (with its memories), GPIO, clock wizard, reset

# CPU clock from clk_wiz: MHz
CPU_CLK_MHZ   = 120

# GPIO address on m_axi_lite
GPIO_BASE     = 0x40000000

# GPIO outputs: 1 to 4 (LD0-LD3)
GPIO_WIDTH    = 4

# Firmware: in sw/fw/simple/
DEFAULT_FW    = led_blink

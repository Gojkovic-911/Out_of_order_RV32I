# Simple platform: CPU, data memory, GPIO, clock wizard, reset
# Format: NAME = value (comments on their own line, never after a value).

# CPU and bus clock out of clk_wiz (MHz)
CPU_CLK_MHZ   = 120

# Address map of the CPU data port (firmware uses these addresses)
DMEM_BASE     = 0x00000000
DMEM_SIZE     = 16K
GPIO_BASE     = 0x40000000
# GPIO register window
GPIO_SIZE     = 64K
# GPIO outputs (LEDs)
GPIO_WIDTH    = 4

# Firmware from sw/fw/simple/ when FW is not given or not found
DEFAULT_FW    = led_blink

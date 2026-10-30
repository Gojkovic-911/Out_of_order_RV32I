# Simple Platform
CPU, 16 KB data memory, GPIO, clock wizard, reset

## Configuration

`simple.mk`:

| Parameter | Default | Description |
|---|---|---|
| `CPU_CLK_MHZ` | 120 | CPU clock |
| `DMEM_BASE` | `0x00000000` | data memory address |
| `DMEM_SIZE` | 16K | data memory size |
| `GPIO_BASE` | `0x40000000` | GPIO address |
| `GPIO_SIZE` | 64K | GPIO address window |
| `GPIO_WIDTH` | 4 | GPIO outputs |
| `DEFAULT_FW` | `led_blink` | firmware from `sw/fw/simple/` |

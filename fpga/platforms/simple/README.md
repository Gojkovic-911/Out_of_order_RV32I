# Simple Platform
CPU (with IMEM & DMEM), GPIO, clock wizard, reset

## Configuration

`simple.mk`:

| Parameter | Default | Description |
|---|---|---|
| `CPU_CLK_MHZ` | 120 | CPU clock |
| `GPIO_BASE` | `0x40000000` | GPIO address |
| `GPIO_WIDTH` | 4 | GPIO outputs |
| `DEFAULT_FW` | `led_blink` | firmware from `sw/fw/simple/` |


### <ins>Implementation Results</ins>

Simple platform:

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 3 176 (75) | 17 600 | 18 % |
| Flip-flop | 2 856 | 35 200 | 8 % |
| Block RAM (36 Kb) | 9.5 | 60 | 16 % |

Timing is met at 120 MHz (worst slack +0.25 ns); estimated power 0.265 W (0.120 W clock wizard MMCM).

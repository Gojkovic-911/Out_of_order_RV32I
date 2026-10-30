# Zybo
Original Zybo, Zynq-7010, PL only

## Configuration

`board.mk`:

| Parameter | Default | Description |
|---|---|---|
| `PART` | `xc7z010clg400-1` | FPGA part |
| `BOARD_CLK_MHZ` | 125 | board oscillator |

Pins in `zybo.xdc`: clock L16, reset BTN0, GPIO LD0-LD3.

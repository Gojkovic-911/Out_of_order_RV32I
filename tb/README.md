# Testbench
CPU and platform testbenches, tests, coverage, simulation scripts

## Configuration

`tb.mk`:

| Parameter | Default | Description |
|---|---|---|
| `TB_CLK_NS` | 100 | CPU tb clock period |
| `IDLE_STOP_CYCLES` | 200 | zero fetches ending a test |
| `MAX_CYCLES` | 1000000 | CPU test limit |
| `DATA_MEM_BITS` | 14 | AXI model 2^bits bytes |
| `DATA_MEM_MIN_WAIT`, `DATA_MEM_MAX_WAIT` | 0, 3 | AXI model random waits |

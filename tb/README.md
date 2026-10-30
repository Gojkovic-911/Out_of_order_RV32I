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
| `DATA_MEM_SEED` | 44257 | wait generator seed |
| `RESET_US` | 1 | platform reset time |
| `BD_MAX_US` | 2000 | platform test limit |
| `BD_IDLE_US` | 5 | zero fetch ending a test |
| `FW_SIM_US` | 200 | firmware run time |

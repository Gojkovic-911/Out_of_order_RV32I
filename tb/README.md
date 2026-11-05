# Testbench
CPU and platform testbenches, tests, coverage, simulation scripts

## Configuration

The `DATA_MEM_MIN/MAX_WAIT` is for modelling variable latency in simulation. To be used with DMEM_RD_LATENCY.

`tb.mk`:

| Parameter | Default | Description |
|---|---|---|
| `TB_CLK_NS`         | 100 | CPU tb clock period |
| `IDLE_STOP_CYCLES`  | 200 | Zero fetches ending a test |
| `MAX_CYCLES`        | 1000000 | CPU test limit |
| `DATA_MEM_MIN_WAIT` | 0 | Minimal delay in cycles |
| `DATA_MEM_MAX_WAIT` | 3 | Maximum delay in cycles |

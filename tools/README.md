# Tools

## Configuration

`tools.mk` holds the machine and tool settings:

| Parameter | Default | Description |
|---|---|---|
| `VIVADO` | `auto` | Vivado path, `auto`: locate |
| `CROSS` | `riscv64-unknown-elf` | RISC-V toolchain prefix |
| `PYTHON` | `/usr/bin/python3` | Python with playwright |
| `CFLAGS` | `-O2 -mstrict-align` | C flags |
| `MAX_JOBS` | 3 | parallel Vivado runs |
| `IMPL_EFFORT` | `performance` | `default` or `performance` |
| `SYNTH_DIRECTIVE` ... `ROUTE_DIRECTIVE` | | step directives |

`make check` checks the tools.

## Caching

Repeated runs reuse what is still valid; `make clean` removes every cache.

| Cache | Location | Rebuilt when | Used by | Time without (with) |
|---|---|---|---|---|
| Golden vectors | `out/tb/tests/stress_tests/<test>/` | `.s` changes (SHA-256) | `make cpu sim`, `make fpga sim` | needs internet |
| CPU simulation snapshot | `out/tb/cpu/sim_np/` | CPU RTL, testbench or generics change | `make cpu sim` | 2 min 47 s (50 s) |
| CPU simulation project | `out/tb/cpu/RISCV_project/` | missing | `make cpu sim PRJ=1` | 3 min 26 s (1 min 37 s) |
| Platform simulation snapshot | `out/tb/fpga/<platform>_<board>/sim/` | RTL, block design, testbench or config change (md5) | `make fpga sim` | 5 min 06 s (1 min 15 s) |
| CPU IP | `out/RV32I/ip_package/` | CPU RTL changes | `make fpga ...` | |
| Xilinx IPs | `out/fpga/ip_cache/` | IP configuration changes | `make fpga bitstream` | 16 min 16 s (~15 min) |

Not cached: `make cpu synth`, the CPU and data memory in `make fpga bitstream` (always synthesized), `make fpga fw-update`.

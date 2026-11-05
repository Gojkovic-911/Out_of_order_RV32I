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
| CPU simulation project | `out/tb/cpu/RISCV_project/` | missing; delete it after adding or moving a source file | `make cpu sim PRJ=1` | 3 min 26 s (1 min 37 s) |
| Platform simulation snapshot | `out/tb/fpga/<platform>_<board>/sim/` | RTL, block design, testbench or config change (md5) | `make fpga sim` | 5 min 06 s (1 min 15 s) |
| CPU IP | `out/RV32I/ip_package/` | CPU RTL or `package_ip.tcl` changes | `make fpga ...` | 32 s |
| SHA3 IP | `out/fpga/ip_package/sha3/` | `fpga/ip/sha3/` RTL or `fpga/rtl/axi_lite_regs.vhd` changes | `make fpga ...` with `FPGA_IPS = sha3` | 31 s |
| Xilinx IPs | `out/fpga/ip_cache/` | IP configuration changes | `make fpga bitstream` | 16 min 16 s (~15 min) |

Not cached: `make cpu synth`, the CPU & local IPs in `make fpga bitstream` are always synthesized, `make fpga fw-update`.


Up to 5 projects generated in total:

1. `out/tb/cpu/RISCV_project`: `make cpu sim PRJ=1`
2. `out/tb/fpga/<platform>_<board>/sim`: `make fpga sim`
3. `out/fpga/<platform>_<board>/project`: `make fpga bitstream`
4. `out/RV32I/ip_package`: CPU IP packaging
5. `out/fpga/ip_package/sha3`: SHA3 IP packaging

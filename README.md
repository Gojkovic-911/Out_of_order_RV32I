# Out_of_order_RV32I
Out‑of‑order RISC‑V (RV32I) core written in VHDL, evolving from a single‑cycle implementation.

## Requirements
gcc-riscv64-unknown-elf


## Commands
```bash
# Simulation (TEST=<name>: one test, default all)
make sim         # <TEST=...>               regression, no project
make sim-prj     # <TEST=...>               regression, Vivado project
make sim-bd      # <TEST=...>               regression on the Zybo block design
make res         # <TEST=...>               last results

# FPGA (PROG=<program>: default last compiled test; settings in RV32I/config.mk)
make ip          #                          package CPU IP
make synth       # <PROG=...> <PERIOD=...>  CPU synthesis, critical paths
make bitstream   # <PROG=...>               Zybo bitstream
make all         # <PROG=...>               sim, then bitstream
make fw          # <FW=...>                 firmware to PROG format (RV32I/fw)
make fw-update   # <FW=...>                 firmware into the built bitstream, no rebuild
make clean       #                          remove generated files
```

## Configuration

`RV32I/config.mk` holds the whole configuration, passed to every tool (simulation, synthesis, IP, block design):

- core sizes (generics in rtl): `NUM_PHYS_REGS`, `ROB_DEPTH`, `IQ_DEPTH`, `LSQ_DEPTH` ...
- simulation: `DATA_MEM_MAX_WAIT` (wait cycles of the AXI data memory model)
- FPGA: `PART`, `MAX_JOBS`, clocks, address map

## Implementation

### CPU 

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 4 145 (174) | 17 600 | 24 % |
| Flip-flop | 3 673 | 35 200 | 10 % |
| Block RAM (36 Kb) | 4.5 | 60 | 8 % |

Timing is met at 50 MHz (worst slack +6.90 ns), maximum clock about 76 MHz; estimated power 0.125 W.

### FPGA build (Zybo)

The platform is the CPU IP, a 16 KB data BRAM (`0x0000_0000`) and a 4-bit GPIO on LD0-LD3 (`0x4000_0000`), clocked at 50 MHz from the 125 MHz board clock; BTN0 is reset.

Whole Zybo platform:

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 4 173 (176) | 17 600 | 24 % |
| Flip-flop | 3 900 | 35 200 | 11 % |
| Block RAM (36 Kb) | 8.5 | 60 | 14 % |
| MMCM / IOB | 1 / 6 | 2 / 100 | |

Timing is met at 50 MHz (worst slack +7.63 ns), maximum clock about 80 MHz; estimated power 0.236 W (0.106 W clock wizard MMCM).

## Adding a new test

1. Open a new folder with the name of your test in `RV32I/tb/tests/`
2. Put `test_name.s` file into it
3. Run `make sim TEST=test_name`

## Run times

Vivado 2023.2, 16-thread machine, 16 GB RAM, 3-4 parallel jobs:

| Command | Time |
|---|---:|
| `make sim` (13 tests, golden vectors cached) | 27 s |
| `make sim-prj` (11 tests) | 3 min 03 s |
| `make sim-bd` (13 tests) | 8 min |
| `make synth` (CPU only) | 3 min 22 s |
| `make bitstream` | ~ 7 min |
| `make fw-update` | 10 s |

## TODO

1. LED blink firmware: a program that writes the GPIO (`0x4000_0000`), baked into the bitstream, to run the core on the Zybo
2. Optimize critical paths
3. SHA3 platform: add a SHA3 accelerator with the DMA
4. More ALUs (useful with multiple issue)
5. Backpressure through valid/ready per stage instead of separate stall logic: whoever is blocked drops ready, everything behind it holds and writes nothing, everything ahead keeps moving
6. Instruction memory on the other side of AXI, loaded over JTAG (no rebuild of the bitstream to change the program)
7. Exception support

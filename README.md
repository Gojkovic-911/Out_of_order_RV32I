# Out_of_order_RV32I
Out‑of‑order RISC‑V (RV32I) core written in VHDL, evolving from a single‑cycle implementation.

## Requirements
gcc-riscv64-unknown-elf


## Commands
Output of `make help`; arguments in `<...>` are optional:

```
Simulation (TEST=<name>: one test, default all):
  make sim        <TEST=...>               regression, no project
  make sim-prj    <TEST=...>               regression, Vivado project
  make res        <TEST=...>               last results

FPGA (PROG=<program>: default last compiled test; settings in RV32I/config.mk):
  make ip                                  package CPU IP
  make synth      <PROG=...> <PERIOD=...>  CPU synthesis, critical paths
  make bitstream  <PROG=...>               Zybo bitstream
  make all        <PROG=...>               sim, then bitstream
  make clean                               remove generated files
```

## Configuration

`RV32I/config.mk` holds the whole configuration, passed to every tool (simulation, synthesis, IP, block design):

- core sizes (generics in rtl): `NUM_PHYS_REGS`, `ROB_DEPTH`, `IQ_DEPTH`, `RS_DEPTH`, `LSQ_DEPTH` ...
- simulation: `DATA_MEM_MAX_WAIT` (wait cycles of the AXI data memory model)
- FPGA: `PART`, `MAX_JOBS`, clocks, address map

## Implementation

### CPU 

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 6 327 (174) | 17 600 | 36 % |
| Flip-flop | 6 792 | 35 200 | 19 % |
| Block RAM (36 Kb) | 4.5 | 60 | 8 % |

Timing is met at 50 MHz (worst slack +3.65 ns), maximum clock about 61 MHz; estimated power 0.140 W.

### FPGA build (Zybo)

The platform is the CPU IP, a 16 KB data BRAM (`0x0000_0000`) and a 4-bit GPIO on LD0-LD3 (`0x4000_0000`), clocked at 50 MHz from the 125 MHz board clock; BTN0 is reset.

Whole Zybo platform:

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 6 580 (176) | 17 600 | 37 % |
| Flip-flop | 6 891 | 35 200 | 20 % |
| Block RAM (36 Kb) | 8.5 | 60 | 14 % |
| MMCM / IOB | 1 / 6 | 2 / 100 | |

Timing is met at 50 MHz (worst slack +3.39 ns), maximum clock about 60 MHz; estimated power 0.248 W (0.106 W clock wizard MMCM).

## Adding a new test

1. Open a new folder with the name of your test in `RV32I/tests/`
2. Put `test_name.s` file into it
3. Run `make sim TEST=test_name`

## Run times

Vivado 2023.2, 16-thread machine, 16 GB RAM, 3-4 parallel jobs:

| Command | Time |
|---|---:|
| `make sim-prj` (11 tests) | 3 min 03 s |
| `make sim` (11 tests) | 2 min 09 s |
| `make synth` (CPU only) | 9 min 42 s |
| `make bitstream` | ~ 13 min |

## TODO

1. LED blink firmware: a program that writes the GPIO (`0x4000_0000`), baked into the bitstream, to run the core on the Zybo
2. Optimize critical paths
3. SHA3 platform: add a SHA3 accelerator with the DMA
4. More ALUs (useful only with multi-issue) and several CDB ports; with single issue a second single-cycle ALU stays idle
5. Backpressure through valid/ready per stage instead of separate stall logic: whoever is blocked drops ready, everything behind it holds and writes nothing, everything ahead keeps moving

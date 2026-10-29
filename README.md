# Out_of_order_RV32I
Out‑of‑order RISC‑V (RV32I) Core in VHDL

## General Info

- Goal: port Zephyr RTOS to the core, run an application on it, then connect the board to the network 
- Flow: everything runs in the Vivado flow (xsim simulation, synthesis, implementation, IP packaging, bitstream)
- Validation: assembly stress tests, register state compared against a golden model
- Components:
  - Fetch with static (always-taken) branch prediction
  - Rename: Rename Table, Commit Rename Table, Free List
  - Reorder Buffer (ROB), branch recovery at commit
  - Instruction Queue (IQ), issues only instructions with both operands ready
  - ALU, Branch Unit, Address Generation Unit
  - Load/Store Queue (LSQ) and Load/Store Unit (LSU)
  - Central Data Bus (CDB)
  - AXI-Lite master to the data memory
  - Instruction memory inside the CPU, updated in the generated bitstream through updatemem

## Requirements

- Vivado 2023.2
- gcc-riscv64-unknown-elf
- System Python 3 (`/usr/bin/python3`) with playwright (`pip install playwright`, `playwright install chromium`) for the golden vectors; the Vivado Python is not used

## Commands
```bash
# Simulation (TEST=<name>: one test, default all)
make sim         # <TEST=...>               regression, no project
make sim-prj     # <TEST=...>               regression, Vivado project
make sim-bd      # <TEST=...>               regression on the CPU platform
make res         # <TEST=...>               last results

# FPGA (PROG=<program>: default last compiled test; settings in RV32I/config.mk)
make ip          #                          package CPU IP
make synth       # <PROG=...> <PERIOD=...>  CPU synthesis & implementation
make bitstream   # <PROG=...>               FPGA bitstream
make all         # <PROG=...>               sim, then bitstream
make fw-update   # <FW=...>                 firmware into the built bitstream, no rebuild
make clean       #                          remove generated files
```

## CPU

### <ins>Configuration</ins>

`RV32I/config.mk`, passed to simulation, synthesis and the IP:

- Generics in rtl: `NUM_PHYS_REGS`, `ROB_DEPTH`, `IQ_DEPTH`, `LSQ_DEPTH` ...
- Simulation: `DATA_MEM_MIN_WAIT`, `DATA_MEM_MAX_WAIT` (random wait cycles of the AXI data memory model; only in the CPU simulation, the platform uses the real BRAM)

### <ins>Implementation Results</ins>

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 3 967 (86) | 17 600 | 23 % |
| Flip-flop | 3 690 | 35 200 | 10 % |
| Block RAM (36 Kb) | 5.5 | 60 | 9 % |

Timing is met at 120 MHz (worst slack +0.03 ns)

Estimated power 0.167 W.

### <ins>Simulation</ins>

`make sim` and `make sim-prj` run the regression on the CPU with an AXI-Lite data memory model.

#### Adding a New Test

1. Open a new folder with the name of your test in `RV32I/tb/tests/`
2. Put `test_name.s` file into it
3. Run `make sim TEST=test_name`

### <ins>Run Times</ins>

Vivado 2023.2, 16-thread machine, 16 GB RAM, 3-4 parallel jobs:

| Command | Time |
|---|---:|
| `make sim` (13 tests, golden vectors cached) | 27 s |
| `make sim-prj` (11 tests) | 3 min 03 s |
| `make synth` (CPU only) | 3 min 22 s |

## FPGA

### <ins>Configuration</ins>

`RV32I/config.mk`, passed to the CPU platform and the bitstream flow:

- `PART`, `MAX_JOBS`, clocks, address map

### <ins>CPU Platform</ins>

The CPU platform is the block design around the CPU IP: a 16 KB data BRAM (`0x0000_0000`) and a 4-bit GPIO on LD0-LD3 (`0x4000_0000`), clocked at 120 MHz from the 125 MHz board clock; BTN0 is reset. 

There is one platform for now.

### <ins>Implementation Results</ins>

Whole CPU platform:

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 4 318 (176) | 17 600 | 24 % |
| Flip-flop | 3 900 | 35 200 | 11 % |
| Block RAM (36 Kb) | 8.5 | 60 | 14 % |
| MMCM / IOB | 1 / 6 | 2 / 100 | |

Timing is met at 120 MHz (worst slack +0.01 ns); estimated power 0.303 W (0.120 W clock wizard MMCM).

### <ins>Simulation</ins>

`make sim-bd` runs the regression on the CPU platform.

### <ins>Changing the Firmware</ins>

The instruction memory is inside the CPU, so the program is part of the bitstream. `make fw-update FW=<name>` assembles `RV32I/fw/<name>.s` and writes it into the built bitstream with updatemem (`out/bitstream/design_1_wrapper_<name>.bit`), without a new synthesis or implementation.

### <ins>Run Times</ins>

| Command | Time |
|---|---:|
| `make sim-bd` (13 tests) | 8 min |
| `make bitstream` | ~ 14 min |
| `fw-update` | 10 s |

## TODO

1. Optimize critical paths
2. SHA3 platform: add a SHA3 accelerator with the DMA
3. Backpressure through valid/ready per stage instead of separate stall logic: whoever is blocked drops ready, everything behind it holds and writes nothing, everything ahead keeps moving. This should be configurable: backpressure or the classic control path
4. Instruction memory on the other side of AXI, loaded over JTAG (no updatemem and no new bitstream download to change the program):
   - Instruction BRAM with two ports: one for the CPU fetch, one through an AXI BRAM Controller
   - JTAG-to-AXI Master on the AXI side
   - CPU held in reset while the program is written
5. Exception support
6. Spike golden model
7. AXI-Full interface

## Debug

If the Makefile does not find Vivado through `locate` (`VIVADO`, `Makefile` line 9), run `sudo updatedb` or give the path directly:

```bash
make sim VIVADO=/tools/Xilinx/Vivado/2023.2/bin/vivado
```

# Out_of_order_RV32I
Out‑of‑order RISC‑V (RV32I) Core in VHDL

## General Info

- Flow: Vivado
- Validation: Assembly stress tests with the register state compared against a golden model
- CPU components: [RV32I/README.md](RV32I/README.md)

## Requirements

- Vivado 2023.2
- gcc-riscv64-unknown-elf

## Commands
```bash
# CPU 
#  TEST=<name> : Test from tb/tests/*/<name.s|.c>, default=regression
make cpu sim        # <TEST=...> <PRJ=1>       Simulation in xsim, PRJ for Vivado project
make cpu synth      # Out-of-context synthesis and implementation

# FPGA 
#  PLATFORM=<name> : Platform from fpga/platforms/<name>,   default=simple
#  BOARD=<name>    : Board    from fpga/boards/<name>,      default=zybo  
#  FW=<name>       : FW       from sw/fw/<name.c>           default=led_blink

make fpga sim       # <TEST=...> <FW=...>      Regression on the CPU platform
make fpga bitstream # <FW=...>                 FPGA Bitstream with the firmware
make fpga fw-update # <FW=...>                 Firmware into the built bitstream

make res            # Status of all runs
make clean          # Remove out/
```

Up to 4 projects generated in total:

1. `out/tb/cpu/RISCV_project`: `make cpu sim PRJ=1`
2. `out/tb/fpga/<platform>_<board>/sim`: `make fpga sim`
3. `out/fpga/<platform>_<board>/project`: `make fpga bitstream`
4. `out/RV32I/ip_package`: CPU IP packaging, used by the FPGA commands

## Configuration

The Makefile reads the following configuration:

- `RV32I/cpu.mk`
- `fpga/platforms/<platform>/<platform>.mk`
- `fpga/boards/<board>/board.mk`
- `tb/tb.mk`
- `tools/tools.mk`

Stops when a value is missing or invalid, and passes every value to the scripts; a value given on the command line (`make cpu synth SYNTH_CLK_MHZ=130`) overrides the file value. 

Each file is described in the README next to it.

## CPU

### <ins>Simulation</ins>

`make cpu sim` runs every test in `tb/tests/*/` on the CPU with an AXI-Lite data memory model.

#### Adding a New Test

1. Open a new folder with the name of your test in `tb/tests/user/` (either `.s` or `.c`)
2. Put your `test_name.s|.c` into it
3. Run `make cpu sim TEST=test_name`

### <ins>Implementation Results</ins>

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 2 968 (74) | 17 600 | 17 % |
| Flip-flop | 2 850 | 35 200 | 8 % |
| Block RAM (36 Kb) | 5.5 | 60 | 9 % |

Timing is met at 120 MHz (worst slack +0.25 ns)

Estimated power 0.137 W.

### <ins>Run Times</ins>

Vivado 2023.2, 16-thread machine, 16 GB RAM, 3-4 parallel jobs:

| Command | Time (cached) |
|---|---:|
| `make cpu sim` (14 tests) | 2 min 47 s (50 s) |
| `make cpu sim PRJ=1` | 3 min 26 s (1 min 37 s) |
| `make cpu synth` | 5 min 13 s |

## FPGA

### <ins>Platform</ins>

- Simple platform is the block design around the CPU IP: a 16 KB data BRAM (`0x0000_0000`) and a 4-bit GPIO on LD0-LD3 (`0x4000_0000`), clocked at 120 MHz from the 125 MHz board clock; BTN0 is reset.

### <ins>Implementation Results</ins>

Simple platform:

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 3 176 (75) | 17 600 | 18 % |
| Flip-flop | 2 856 | 35 200 | 8 % |
| Block RAM (36 Kb) | 9.5 | 60 | 16 % |

Timing is met at 120 MHz (worst slack +0.16 ns); estimated power 0.261 W (0.120 W clock wizard MMCM).

### <ins>Simulation</ins>

`make fpga sim` runs the regression on the Simple platform; `make fpga sim FW=<name>` runs the firmware for a fixed time and saves the waveform.

### <ins>Changing the Firmware</ins>

The instruction memory is inside the CPU, so the program is part of the bitstream. `make fpga fw-update FW=<name>` builds `sw/fw/<platform>/<name>.c` and writes its code and data into the built bitstream with updatemem into `out/fpga/<platform>_<board>/bitstream/design_1_wrapper_<name>.bit`

### <ins>Run Times</ins>

| Command | Time (cached) |
|---|---:|
| `make fpga sim` (14 tests) | 5 min 06 s (1 min 15 s) |
| `make fpga bitstream` | 16 min 16 s (~15 min) |
| `make fpga fw-update` | 17 s |

## TODO

1. Optimize critical paths
2. SHA3 platform: add a SHA3 accelerator with the DMA
3. Stall: Backpressure through valid/ready per stage
4. Instruction and data memory loaded over JTAG
5. Exception support
6. Spike golden model
7. AXI-Full interface

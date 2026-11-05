# Out_of_order_RV32I
Out‑of‑order RISC‑V (RV32I) Core in VHDL

## General Info

- Goal: Port Zephyr and connect to the internet
- Flow: Vivado
- Validation: Stress testing & CPU state verified against a golden model

## CPU

### <ins>Implementation Results</ins>

| Resource | Used | Available | % |
|---|---:|---:|---:|
| LUT (of which LUTRAM) | 3 302 (118) | 17 600 | 19 % |
| Flip-flop | 2 981 | 35 200 | 8 % |
| BRAM (36 Kb) | 9.5 | 60 | 16 % |

Timing is met at 120 MHz (worst slack +0.45 ns)

Estimated power 0.137 W.

### <ins>Simulation</ins>

`make cpu sim` runs every test in `tb/tests/*/` on the CPU.

#### Adding a New Test

1. Open a new directory with the name of your test in `tb/tests/user/` (either `.s` or `.c`)
2. Put your `test_name.s|.c` into it
3. Run `make cpu sim TEST=test_name`

## FPGA

### <ins>Platforms</ins>

Platforms are the block designs around the CPU IP:
1. Simple platform - platform with the CPU and GPIO with LED-blinking firmware
2. SHA3 platform - platform with the CPU, DMA and SHA3 engine (engine made a long time ago)

### <ins>Simulation</ins>

`make fpga sim` runs the regression on the selected platform;

`make fpga sim FW=<name>` runs the firmware and saves the waveform.

### <ins>Changing the Firmware</ins>

Both instruction and data memory are inside the CPU. 

`make fpga fw-update PLATFORM=<platform> FW=<fw>` builds `sw/fw/<platform>/<fw>.c` and writes the code and data into the built bitstream with updatemem into `out/fpga/<platform>_<board>/bitstream/design_1_wrapper_<fw>.bit`

## Runs

### Requirements

- Vivado 2023.2
- gcc-riscv64-unknown-elf

### Commands

```bash
# CPU 
#  TEST=<name> : Test from tb/tests/*/<name.s|.c>, default=regression
make cpu sim        # <TEST=...> <PRJ=1>       Simulation in xsim, PRJ for Vivado project
make cpu synth      # Out-of-context synthesis and implementation

# FPGA 
#  PLATFORM=<name> : Platform from fpga/platforms/<name>,   default=sha3
#  BOARD=<name>    : Board    from fpga/boards/<name>,      default=zybo  
#  FW=<name>       : FW       from sw/fw/<platform>/<name>.c, default defined in <platform>.mk

make fpga sim       # <TEST=...> <FW=...>      Regression on the CPU platform
make fpga bitstream # <FW=...>                 FPGA Bitstream with the firmware
make fpga fw-update # <FW=...>                 Firmware update in the bitstream using updatemem

make res            # Status of all runs
make clean          # Remove out/
```

### Configuration

The Makefile reads the following configuration:

- `RV32I/cpu.mk`
- `fpga/platforms/<platform>/<platform>.mk`
- `fpga/boards/<board>/board.mk`
- `tb/tb.mk`
- `tools/tools.mk`

Passes every value to the scripts; a value given on the command line (`make cpu synth SYNTH_CLK_MHZ=130`) overrides the file value.

### <ins>Run Times</ins>

Vivado 2023.2, 16-thread machine, 16 GB RAM, 3 parallel jobs:

| Command | Time (cached) |
|---|---:|
| `make cpu sim` | ~3 min (~1 min) |
| `make cpu synth` | ~4 min |
| `make fpga sim` | ~5 min (~1 min) |
| `make fpga bitstream` | ~20 min (~19 min) |
| `make fpga fw-update` | ~20 s |

## TODO

1. Optimize critical paths
2. Instruction and data memory loaded over JTAG
3. Exception support
4. Spike golden model
5. Master AXI-Full interface
6. Interrupts controller
7. Get CoreMark/MHz & more metrics for the CPU

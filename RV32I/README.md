# RV32I CPU
Out-of-order RV32I core, configured in [cpu.mk](cpu.mk)

## Components

- Fetch with static (always-taken) branch prediction
- Rename: Rename Table, Commit Rename Table, Free List
- Reorder Buffer (ROB), branch recovery at commit
- Instruction Queue (IQ), issues only instructions with both operands ready
- ALU, Branch Unit, Address Generation Unit
- Load/Store Queue (LSQ) and Load/Store Unit (LSU)
- Central Data Bus (CDB)
- AXI-Lite master to the data memory
- Instruction memory inside the CPU, updated in the generated bitstream through updatemem

## Configuration

`cpu.mk`:

| Parameter | Default | Description |
|---|---|---|
| `DATA_WIDTH`, `MEM_BYTES` | 32, 4 | data width |
| `ARCH_ADDR_BITS` | 5 | 32 registers |
| `NUM_PHYS_REGS` | 64 | physical registers |
| `ROB_DEPTH` | 32 | ROB entries |
| `IQ_DEPTH` | 8 | IQ entries |
| `LSQ_DEPTH` | 8 | LSQ entries |
| `WADDR` | 14 | instruction memory 2^WADDR bytes |
| `RF_RAM_STYLE` | `auto` | register file RAM |
| `IQ_RAM_STYLE` | `distributed` | IQ payload RAM |
| `SYNTH_CLK_MHZ` | 120 | `cpu synth` clock, `platform`: CPU_CLK_MHZ |
| `SYNTH_PART` | `xc7z010clg400-1` | `cpu synth` part, `board`: PART |

RAM styles: `auto`, `distributed`, `block`, `registers`.

## Sweep

A tool to choose the default configuration, not part of the normal flow:

```bash
make sweep                              # sizes and RF/IQ RAM styles (hours)
python3 RV32I/scripts/sweep.py --directives  # place x route directives on cpu.mk (about 10 h)
make impl                               # place and route only, from the last bitstream synthesis
```

- Sizes: every ROB/IQ/LSQ combination is simulated (PASS, IPC); CPU synthesis per dimension and for 4 RF/IQ RAM style pairs
- Directives: the first run is `make fpga bitstream`, the rest are `make impl` with `PLACE_DIRECTIVE` / `ROUTE_DIRECTIVE`
- Report: `out/RV32I/sweep/results.md`; finished points are cached in csv files, so a stopped sweep continues

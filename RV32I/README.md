# RV32I CPU
Out-of-order RV32I core, configured in [cpu.mk](cpu.mk)

## Components

- Fetch with static (always-taken) branch prediction
- Rename: Rename Table, Commit Rename Table, Free List
- Reorder Buffer (ROB), 
- Instruction Queue (IQ), issues only instructions with both operands ready
- ALU, Integer
- Branch Unit, branch recovery at commit
- Primitive Address Generation Unit
- Load/Store Queue (LSQ) and Load/Store Unit (LSU)
- Central Data Bus (CDB)
- Instruction & Data TCMs inside the CPU, updated in the generated bitstream through updatemem

## Configuration

`cpu.mk`:

| Parameter | Default | Description |
|---|---|---|
| `DATA_WIDTH` | 32 | data width |
| `ARCH_ADDR_BITS` | 5 | 32 registers |
| `NUM_PHYS_REGS` | 64 | physical registers |
| `ROB_DEPTH` | 32 | ROB entries |
| `IQ_DEPTH` | 8 | IQ entries |
| `LSQ_DEPTH` | 8 | LSQ entries |
| `IMEM_ADDR_BITS` | 14 | Instruction Memory size |
| `RF_RAM_STYLE` | `auto` | register file RAM |
| `IQ_RAM_STYLE` | `distributed` | IQ payload RAM |
| `SYNTH_CLK_MHZ` | 120 | `cpu synth` clock, `platform`: CPU_CLK_MHZ |
| `SYNTH_PART` | `xc7z010clg400-1` | `cpu synth` part, `board`: PART |
| `DMEM_BASE` | `0x00000000` | Data Memory starting address |
| `DMEM_ADDR_BITS` | `14` | Data Memory Size |
| `DMEM_RD_LATENCY` | `1` | Memory Output Pipeline depth - fixed latency in rtl (Variable latency in tb/tb.mk) |

RAM styles: `auto`, `distributed`, `block`, `registers`.

## Sweep

A tool to explore different configurations:

```bash
make sweep                              # sizes and RF/IQ RAM styles (hours)
python3 RV32I/scripts/sweep.py --directives  # place x route directives on cpu.mk (about 10 h)
make impl                               # place and route only, on the cached synthesis run (out/fpga/<platform>_<board>/project/.../synth_1)
```

- The regression runs on every ROB/IQ/LSQ combination; CPU synthesis runs on the base point, one dimension at a time, the all-min to all-max diagonal and the 4 RF/IQ RAM style pairs
- `--directives` then runs every place x route directive pair on the configuration in `cpu.mk` (write the chosen sizes there first)
- Report: `out/RV32I/sweep/results.md`; finished points are cached in csv files, so a stopped sweep can continue

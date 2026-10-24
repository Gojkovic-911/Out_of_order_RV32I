# Out_of_order_RV32I

Out‑of‑order RISC‑V (RV32I) core written in VHDL, evolving from a single‑cycle implementation.

## Requirements
gcc-riscv64-unknown-elf


## Quick Run

```bash
make mispredict_test   # Runs assembly, simulation, and register verification
```
## Regression run

```bash
make   # Runs a regression of all the tests
```

## Non-project regression run

```bash
make np   # Same regression without a Vivado project (xvhdl / xelab / xsim directly)
```

The simulation snapshot is built in `RV32I/sim_np/` and rebuilt only when a VHDL source changes; each test's program is loaded by restarting the simulation. Use `make` when you need the Vivado project (GUI, waveforms), `make np` for fast regression runs.

## Adding a new test

1. Open a new folder with the name of your test in `RV32I/RISCV_tb/`
2. Put `test_name.s` file into it
3. Run `make test_name`

## Regression run times

Full regression of 11 tests, Vivado 2023.2, 16-core machine (measured 2026-10-24):

| Command | State | Total | Golden model | Simulation + tool |
|---|---|---:|---:|---:|
| `make`    | project created from scratch | 3 min 04 s | ~104 s | ~80 s |
| `make`    | project already exists       | 3 min 00 s | ~104 s | ~76 s |
| `make np` | snapshot built (~10 s)       | 2 min 26 s | ~104 s | ~42 s |
| `make np` | snapshot up to date          | 2 min 19 s | ~104 s | ~35 s |

- The golden model (Cornell RISC-V interpreter in a headless browser) takes about three quarters of the run, so it is the first thing to speed up.
- Without it, `make np` is about twice as fast as `make`: the difference is starting Vivado, opening the project and `launch_simulation` on every run.
- A second `make` run saves almost nothing, since `launch_simulation` goes through compilation and elaboration each time.

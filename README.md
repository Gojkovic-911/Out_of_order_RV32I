# Out_of_order_RV32I

Out‑of‑order RISC‑V (RV32I) core written in VHDL, evolving from a single‑cycle implementation.

## Requirements
gcc-riscv64-unknown-elf


## Quick Run

```bash
make single_missp_test   # Runs assembly, simulation, and register verification
```
## Regression run

```bash
make   # Runs a regression of all the tests
```

## Adding a new test

1. Open a new folder with the name of your test in `RV32I/RISCV_tb/`
2. Put `test_name.s` file into it
3. Run `make test_name`

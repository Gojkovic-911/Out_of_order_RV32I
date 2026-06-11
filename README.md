# Out_of_order_RV32I

Out‑of‑order RISC‑V (RV32I) core written in VHDL, evolving from a single‑cycle implementation.

## Requirements
gcc-riscv64-unknown-elf


## Quick Run

```bash
make single_missp_test   # runs assembly, simulation, and register verification
```
## Adding a new test

1. Open a new folder with the name of your test in `RV32I/RISCV_tb/`
2. Put `test_name.s` file into it
3. Run `make test_name`

## Regression run

```bash
make   # runs a mini regression of 3 tests
```

# Makefile for running all tests under RV32I/RISCV_tb/*test

SHELL = /bin/bash
TEST_DIR = RV32I/RISCV_tb
MASTER_SCRIPT = ./master_script.sh

# Find all directories ending with "test"
TEST_NAMES = $(shell find $(TEST_DIR) -maxdepth 1 -type d -name '*test' -exec basename {} \;)

.PHONY: all $(TEST_NAMES)

all: $(TEST_NAMES)

# Run each test
$(TEST_NAMES):
	@echo "Running test: $@"
	$(MASTER_SCRIPT) $@

# Optional: run a specific test by name (e.g., make single_missp_test)
%:
	@echo "Running test: $@"
	$(MASTER_SCRIPT) $@

# Clean any generated files (if needed)
clean:
	find . -name "*.txt" -type f | xargs rm -f
	rm -rf RV32I/vivado/RISCV_project *.log *.jou .Xil RV32I/vivado/.Xil

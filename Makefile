# Makefile for running tests under RV32I/RISCV_tb/*test

SHELL = /bin/bash
VIVADO_SCRIPT = RV32I/vivado/RISCV.tcl

# Pronađi Vivado instalaciju
VIVADO = $(shell locate bin/vivado | grep '/bin/vivado$$' | head -1)

# Provjeri da li je Vivado pronađen
ifeq ($(strip $(VIVADO)),)
    VIVADO = ~/Public/Vivado/2023.2/bin/vivado
endif

.PHONY: all clean results

# Default target: run regression (no argument passed)
all: regression

# Run regression (when no test name is given)
regression:
	@echo "========================================="
	@echo "Running REGRESSION on all tests"
	@echo "========================================="
	export TEST_NAME=regression; \
	$(VIVADO) -mode batch -source $(VIVADO_SCRIPT) -tclargs "$$TEST_NAME" -nojournal -nolog
	@./RV32I/scripts/verdict.sh

# Run a specific test (when a test name is given as argument)
# e.g., make single_missp_test
%:
	@echo "========================================="
	@echo "Running single test: $@"
	@echo "========================================="
	export TEST_NAME=$@; \
	$(VIVADO) -mode batch -source $(VIVADO_SCRIPT) -tclargs "$$TEST_NAME" -nojournal -nolog
	@./RV32I/scripts/verdict.sh $@

results:
	@./RV32I/scripts/verdict.sh

# Clean generated files
clean:
	find . -name "*.txt" -type f | xargs rm -f
	find . -name "*.log" -type f | xargs rm -f
	find . -name "*.jou" -type f | xargs rm -f
	rm -rf RV32I/vivado/RISCV_project .Xil RV32I/vivado/.Xil
	@echo "Clean finished"
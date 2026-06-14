# Makefile for running tests under RV32I/RISCV_tb/*test

SHELL = /bin/bash
VIVADO_SCRIPT = RV32I/vivado/RISCV.tcl

# Pronađi Vivado instalaciju
VIVADO = $(shell locate bin/vivado | grep '/bin/vivado$$' | head -1)

# Provjeri da li je Vivado pronađen
ifeq ($(strip $(VIVADO)),)
    VIVADO = ~/Public/Vivado/2023.2/bin/vivado
endif

.PHONY: all clean res regression

# Default target: run regression (no argument passed)
all: regression

# Run regression (when no test name is given) with time measurement
regression:
	@echo "========================================="
	@echo "Running REGRESSION on all tests"
#	@echo "Start time: $$(date '+%Y-%m-%d %H:%M:%S')"
	@echo "========================================="
	@START_TIME=$$(date +%s); \
	export TEST_NAME=regression; \
	$(VIVADO) -mode batch -source $(VIVADO_SCRIPT) -tclargs "$$TEST_NAME" -nojournal -nolog; \
	END_TIME=$$(date +%s); \
	ELAPSED=$$((END_TIME - START_TIME)); \
	echo "========================================="; \
	echo "REGRESSION COMPLETE"; \
#	echo "End time: $$(date '+%Y-%m-%d %H:%M:%S')"; \
	echo "Total time: $$((ELAPSED / 60)) minutes and $$((ELAPSED % 60)) seconds"; \
	echo "========================================="; 
	@./RV32I/scripts/verdict.sh

# Run a specific test (when a test name is given as argument)
%:
	@echo "========================================="
	@echo "Running single test: $@"
#	@echo "Start time: $$(date '+%Y-%m-%d %H:%M:%S')"
	@echo "========================================="
	@START_TIME=$$(date +%s); \
	export TEST_NAME=$@; \
	$(VIVADO) -mode batch -source $(VIVADO_SCRIPT) -tclargs "$$TEST_NAME" -nojournal -nolog; \
	END_TIME=$$(date +%s); \
	ELAPSED=$$((END_TIME - START_TIME)); \
	echo "========================================="; \
	echo "TEST $@ COMPLETE"; \
#	echo "End time: $$(date '+%Y-%m-%d %H:%M:%S')"; \
	echo "Total time: $$((ELAPSED / 60)) minutes and $$((ELAPSED % 60)) seconds"; \
	echo "========================================="; 
	@./RV32I/scripts/verdict.sh

# Show test results summary
res:
	@./RV32I/scripts/verdict.sh

# Clean generated files
clean:
	find . -name "*.txt" -type f | xargs rm -f
	find . -name "*.log" -type f | xargs rm -f
	find . -name "*.jou" -type f | xargs rm -f
	find . -name "*.dis" -type f | xargs rm -f
	find . -name "*.str" -type f | xargs rm -f
	rm -rf RV32I/vivado/RISCV_project .Xil RV32I/vivado/.Xil
	@echo "Clean finished"
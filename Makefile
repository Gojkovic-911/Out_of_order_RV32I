# Makefile for running tests under RV32I/RISCV_tb/*test

SHELL = /bin/bash
VIVADO_SCRIPT = RV32I/vivado/RISCV.tcl

# Locate Vivado installation
VIVADO = $(shell locate bin/vivado | grep '/bin/vivado$$' | head -1)

.PHONY: all clean res regression np

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

# -------------------- Non-project mode --------------------
# Same regression without a Vivado project: xvhdl/xelab build a snapshot once
# (again only when a source changes), xsim then swaps the program per test
XBIN        = $(dir $(VIVADO))
NP_DIR      = RV32I/sim_np
NP_SNAPSHOT = tb_np
NP_STAMP    = $(NP_DIR)/.$(NP_SNAPSHOT).stamp
NP_PROGRAM  = $(CURDIR)/RV32I/RISCV_tb/assembly_code_bin.txt

# Compile order: packages and decoders first
NP_SRCS = \
	RV32I/rtl/packages/alu_ops_pkg.vhd \
	RV32I/rtl/packages/instr_types_pkg.vhd \
	RV32I/rtl/packages/txt_util.vhd \
	RV32I/rtl/BRAM_byte_addressable.vhd \
	RV32I/rtl/control_path/ctrl_decoder.vhd \
	RV32I/rtl/control_path/alu_decoder.vhd \
	RV32I/rtl/control_path/branch_predictor.vhd \
	RV32I/rtl/control_path/control_path.vhd \
	RV32I/rtl/data_path/ALU_simple.vhd \
	RV32I/rtl/data_path/LSU.vhd \
	RV32I/rtl/data_path/LSQ.vhd \
	RV32I/rtl/data_path/branch_decision_unit.vhd \
	RV32I/rtl/data_path/immediate.vhd \
	RV32I/rtl/data_path/register_bank.vhd \
	RV32I/rtl/data_path/free_list_fifo.vhd \
	RV32I/rtl/data_path/renaming_module.vhd \
	RV32I/rtl/data_path/ROB.vhd \
	RV32I/rtl/data_path/dispatch_module.vhd \
	RV32I/rtl/data_path/execute_unit.vhd \
	RV32I/rtl/data_path/data_path.vhd \
	RV32I/rtl/TOP_RISCV.vhd \
	RV32I/rtl/axi_lite_master.vhd \
	RV32I/rtl/riscv_top_mem.vhd \
	RV32I/RISCV_tb/axi_lite_mem_model.vhd \
	RV32I/RISCV_tb/TOP_RISCV_tb.vhd

$(NP_STAMP): $(NP_SRCS)
	@echo "Building simulation snapshot (non-project)"
	@mkdir -p $(NP_DIR)
	@cd $(NP_DIR) && $(XBIN)xvhdl -2008 $(addprefix $(CURDIR)/,$(NP_SRCS)) > /dev/null || { grep -E 'ERROR' xvhdl.log; exit 1; }
	@cd $(NP_DIR) && $(XBIN)xelab -debug typical -generic_top "PROGRAM_FILE=$(NP_PROGRAM)" work.TOP_RISCV_tb -s $(NP_SNAPSHOT) > /dev/null || { grep -E 'ERROR' xelab.log; exit 1; }
	@touch $@

np: $(NP_STAMP)
	@echo "========================================="
	@echo "Running REGRESSION on all tests (non-project mode)"
	@echo "========================================="
	@START_TIME=$$(date +%s); \
	cd $(NP_DIR) && $(XBIN)xsim $(NP_SNAPSHOT) -tclbatch $(CURDIR)/RV32I/vivado/run_np.tcl; \
	END_TIME=$$(date +%s); \
	ELAPSED=$$((END_TIME - START_TIME)); \
	echo "========================================="; \
	echo "REGRESSION COMPLETE"; \
	echo "Total time: $$((ELAPSED / 60)) minutes and $$((ELAPSED % 60)) seconds"; \
	echo "========================================="
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
	rm -rf RV32I/vivado/RISCV_project .Xil RV32I/vivado/.Xil $(NP_DIR)
	@echo "Clean finished"
# Simulation and Zybo FPGA build

SHELL = /bin/bash
VIVADO_SCRIPT = RV32I/vivado/RISCV.tcl

VIVADO = $(shell locate bin/vivado | grep '/bin/vivado$$' | head -1)

TEST ?=
TEST_DIR = RV32I/tests

# Shared CPU and board configuration
CONFIG ?= RV32I/config.mk
include $(CONFIG)
export CONFIG_MK := $(abspath $(CONFIG))

.PHONY: help all clear-results sim sim-prj sim-bd res ip synth bitstream sweep fw fw-update clean FORCE

# Print targets and options
help:
	@echo "Simulation (TEST=<name>: one test, default all):"
	@echo "  make sim        <TEST=...>               regression, no project"
	@echo "  make sim-prj    <TEST=...>               regression, Vivado project"
	@echo "  make sim-bd     <TEST=...>               regression on the Zybo block design"
	@echo "  make res        <TEST=...>               last results"
	@echo ""
	@echo "FPGA (PROG=<program>: default last compiled test; settings in RV32I/config.mk):"
	@echo "  make ip                                  package CPU IP"
	@echo "  make synth      <PROG=...> <PERIOD=...>  CPU synthesis, critical paths"
	@echo "  make bitstream  <PROG=...>               Zybo bitstream"
	@echo "  make all        <PROG=...>               sim, then bitstream"
	@echo "  make sweep                               size sweep (~3 h)"
	@echo "  make fw         <FW=...>                 firmware to PROG format (RV32I/fw)"
	@echo "  make fw-update  <FW=...>                 firmware into the built bitstream, no rebuild"
	@echo "  make clean                               remove generated files"

# Regression, then bitstream
all: sim
	@$(MAKE) --no-print-directory bitstream

# Remove old test results
clear-results:
	@rm -f $(addprefix $(TEST_DIR)/$(if $(TEST),$(TEST),*test)/,comparison_results.txt ipc.txt)

# Non-project simulation
XBIN        = $(dir $(VIVADO))
NP_DIR      = RV32I/sim_np
NP_SNAPSHOT = tb_np
NP_STAMP    = $(NP_DIR)/.$(NP_SNAPSHOT).stamp
NP_PROGRAM  = $(CURDIR)/RV32I/tests/assembly_code_bin.txt
NP_GENERICS = $(foreach g,$(CORE_GENERICS) $(SIM_GENERICS),-generic_top "$(g)=$($(g))")

# Sources in compile order
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
	RV32I/rtl/data_path/issue_module.vhd \
	RV32I/rtl/data_path/ex_router.vhd \
	RV32I/rtl/data_path/alu_unit.vhd \
	RV32I/rtl/data_path/branch_unit.vhd \
	RV32I/rtl/data_path/cdb.vhd \
	RV32I/rtl/data_path/data_path.vhd \
	RV32I/rtl/axi_lite_master.vhd \
	RV32I/rtl/riscv_top.vhd \
	RV32I/rtl/riscv_top_mem.vhd \
	RV32I/tb/axi_lite_mem_model.vhd \
	RV32I/tb/riscv_top_tb.vhd

# Configuration of the last snapshot
$(NP_DIR)/config.used: FORCE
	@mkdir -p $(NP_DIR)
	@cmp -s $(CONFIG) $@ || cp $(CONFIG) $@

# Build simulation snapshot
$(NP_STAMP): $(NP_SRCS) $(NP_DIR)/config.used
	@echo "Building simulation snapshot (non-project)"
	@mkdir -p $(NP_DIR)
	@cd $(NP_DIR) && $(XBIN)xvhdl -2008 $(addprefix $(CURDIR)/,$(NP_SRCS)) > /dev/null || { grep -E 'ERROR' xvhdl.log; exit 1; }
	@cd $(NP_DIR) && $(XBIN)xelab -debug typical $(NP_GENERICS) -generic_top "PROGRAM_FILE=$(NP_PROGRAM)" work.riscv_top_tb -s $(NP_SNAPSHOT) > /dev/null || { grep -E 'ERROR' xelab.log; exit 1; }
	@touch $@

# Regression without project
sim: $(NP_STAMP) clear-results
	@START_TIME=$$(date +%s); \
	cd $(NP_DIR) && TEST=$(TEST) $(XBIN)xsim $(NP_SNAPSHOT) -tclbatch $(CURDIR)/RV32I/vivado/run_np.tcl; \
	ELAPSED=$$(( $$(date +%s) - START_TIME )); \
	echo "Simulation time: $$((ELAPSED / 60)) min $$((ELAPSED % 60)) s"
	@./RV32I/scripts/verdict.sh $(TEST)

# Regression through the project
sim-prj: clear-results
	@START_TIME=$$(date +%s); \
	$(VIVADO) -mode batch -source $(VIVADO_SCRIPT) -tclargs $(if $(TEST),$(TEST),regression) -nojournal -nolog; \
	ELAPSED=$$(( $$(date +%s) - START_TIME )); \
	echo "Simulation time: $$((ELAPSED / 60)) min $$((ELAPSED % 60)) s"
	@./RV32I/scripts/verdict.sh $(TEST)

# FPGA build (Zybo)
VIVADO_DIR = RV32I/vivado
PROG      ?= $(CURDIR)/RV32I/tests/assembly_code_bin.txt
PERIOD    ?=

# CPU IP packaging
ip:
	@START_TIME=$$(date +%s); \
	$(VIVADO) -mode batch -nojournal -log $(VIVADO_DIR)/ip_package.log -source $(VIVADO_DIR)/package_ip.tcl > /dev/null || { grep -E '^ERROR' $(VIVADO_DIR)/ip_package.log; exit 1; }; \
	echo "IP packaged in $$(( $$(date +%s) - START_TIME )) s"

# Out-of-context synthesis
synth:
	@mkdir -p $(VIVADO_DIR)/synth_out
	@START_TIME=$$(date +%s); \
	$(VIVADO) -mode batch -nojournal -log $(VIVADO_DIR)/synth_out/vivado.log -source $(VIVADO_DIR)/synth_ooc.tcl -tclargs $(abspath $(PROG)) $(PERIOD) > /dev/null || { grep -E '^ERROR' $(VIVADO_DIR)/synth_out/vivado.log; exit 1; }; \
	echo "Synthesis and implementation done in $$(( $$(date +%s) - START_TIME )) s"
	@python3 RV32I/scripts/critical_paths.py $(VIVADO_DIR)/synth_out/paths.txt > $(VIVADO_DIR)/synth_out/critical_paths.txt
	@head -11 $(VIVADO_DIR)/synth_out/critical_paths.txt

# Zybo bitstream
bitstream: ip
	@mkdir -p $(VIVADO_DIR)/bitstream_out
	@START_TIME=$$(date +%s); \
	$(VIVADO) -mode batch -nojournal -log $(VIVADO_DIR)/bitstream_out/vivado.log -source $(VIVADO_DIR)/bitstream.tcl -tclargs $(abspath $(PROG)) > /dev/null || { grep -E '^ERROR' $(VIVADO_DIR)/bitstream_out/vivado.log; exit 1; }; \
	echo "Bitstream done in $$(( $$(date +%s) - START_TIME )) s"
	@python3 RV32I/scripts/critical_paths.py $(VIVADO_DIR)/bitstream_out/paths.txt > $(VIVADO_DIR)/bitstream_out/critical_paths.txt
	@head -11 $(VIVADO_DIR)/bitstream_out/critical_paths.txt

# Size sweep: IPC everywhere, synth on a subset
sweep:
	@python3 RV32I/scripts/sweep.py

# Regression on the Zybo block design
sim-bd: ip clear-results
	@START_TIME=$$(date +%s); \
	TEST=$(TEST) $(VIVADO) -mode batch -nojournal -log $(VIVADO_DIR)/sim_bd.log -source $(VIVADO_DIR)/sim_bd.tcl > /dev/null || { grep -E '^ERROR' $(VIVADO_DIR)/sim_bd.log; exit 1; }; \
	ELAPSED=$$(( $$(date +%s) - START_TIME )); \
	echo "Block design simulation time: $$((ELAPSED / 60)) min $$((ELAPSED % 60)) s"
	@./RV32I/scripts/verdict.sh $(TEST)

# Firmware to instruction memory format
FW     ?= led_blink
FW_DIR  = RV32I/fw
fw:
	@riscv64-unknown-elf-as -march=rv32i $(FW_DIR)/$(FW).s -o $(FW_DIR)/$(FW).o
	@riscv64-unknown-elf-objdump -d $(FW_DIR)/$(FW).o | awk '/^ +[0-9a-f]+:/{print $$2}' | perl -ne 'chomp; printf "%032b\n", hex($$_)' > $(FW_DIR)/$(FW).bin
	@riscv64-unknown-elf-ld -m elf32lriscv -Ttext=0 -e 0 $(FW_DIR)/$(FW).o -o $(FW_DIR)/$(FW).elf
	@rm -f $(FW_DIR)/$(FW).o
	@echo "Firmware: $(FW_DIR)/$(FW).bin, $(FW_DIR)/$(FW).elf"

# Firmware into the built bitstream (no rebuild)
BIT_DIR = $(VIVADO_DIR)/bitstream_out
fw-update: fw
	@$(XBIN)updatemem -force -meminfo $(BIT_DIR)/imem.mmi -data $(FW_DIR)/$(FW).elf -proc imem \
		-bit $(BIT_DIR)/design_1_wrapper.bit -out $(BIT_DIR)/design_1_wrapper_$(FW).bit > $(BIT_DIR)/updatemem.log || { grep -E 'ERROR' $(BIT_DIR)/updatemem.log; exit 1; }
	@echo "Bitstream: $(BIT_DIR)/design_1_wrapper_$(FW).bit"

# Show test results
res:
	@./RV32I/scripts/verdict.sh $(TEST)

# Clean generated files
GEN_FILES = *.txt *.log *.jou *.dis *.str *.hex *.bin *.o *.pb *.vcd *.sha *.elf
GEN_DIRS  = RISCV_project ip_package ip_repo synth_out zybo_project zybo_sim bitstream_out sweep_out .Xil

clean:
	find . -path ./.git -prune -o -type f \( $(foreach f,$(GEN_FILES),-name "$(f)" -o) -false \) -print | xargs rm -f
	find . -path ./.git -prune -o -type d \( $(foreach d,$(GEN_DIRS),-name "$(d)" -o) -false \) -print | xargs rm -rf
	rm -rf $(NP_DIR)
	@echo "Clean finished"
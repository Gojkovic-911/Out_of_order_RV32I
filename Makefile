# CPU and FPGA flows: make cpu <action>, make fpga <action>

SHELL = /bin/bash
TCL_DIR       = RV32I/scripts/tcl
FPGA_TCL      = fpga/scripts
TB_SCRIPTS    = tb/scripts
TB_TCL        = $(TB_SCRIPTS)/tcl
VIVADO_SCRIPT = $(TB_TCL)/RISCV.tcl

# FPGA platform and board (fpga only)
PLATFORM ?= simple
BOARD    ?= zybo
export PLATFORM BOARD

# Generated files, same layout as the repository
CPU_OUT       = out/RV32I
FPGA_OUT      = out/fpga/$(PLATFORM)_$(BOARD)
TB_OUT        = out/tb
CPU_SIM_OUT   = $(TB_OUT)/cpu
BD_SIM_OUT    = $(TB_OUT)/fpga/$(PLATFORM)_$(BOARD)
FW_OUT        = out/sw/fw/$(PLATFORM)
TEST_DIR      = $(TB_OUT)/tests

# CPU, platform, board, tools and testbench configuration
CONFIG    ?= RV32I/cpu.mk
CFG_FILES  = $(CONFIG) fpga/platforms/$(PLATFORM)/$(PLATFORM).mk fpga/boards/$(BOARD)/board.mk tools/tools.mk tb/tb.mk
include $(CFG_FILES)
export CONFIG_MK := $(abspath $(CONFIG))

# Every value to the scripts as CFG_<name>, command line overrides included
CFG_NAMES := $(shell sed -n -E 's/^\s*(\w+)\s*=.*/\1/p' $(CFG_FILES))
$(foreach v,$(CFG_NAMES),$(eval export CFG_$(v) = $$($(v))))

# Required values per file, then allowed values
REQ_CPU      = CORE_GENERICS DATA_WIDTH ARCH_ADDR_BITS NUM_PHYS_REGS ROB_DEPTH IQ_DEPTH LSQ_DEPTH WADDR \
               SYNTH_GENERICS RF_RAM_STYLE IQ_RAM_STYLE SYNTH_CLK_MHZ SYNTH_PART
REQ_PLATFORM = CPU_CLK_MHZ DMEM_BASE DMEM_SIZE GPIO_BASE GPIO_WIDTH DEFAULT_FW
REQ_BOARD    = PART BOARD_CLK_MHZ
REQ_TOOLS    = VIVADO CROSS PYTHON CFLAGS MAX_JOBS IMPL_EFFORT SYNTH_DIRECTIVE OPT_DIRECTIVE PLACE_DIRECTIVE \
               PHYS_OPT_DIRECTIVE ROUTE_DIRECTIVE
REQ_TB       = TB_GENERICS TB_CLK_NS IDLE_STOP_CYCLES MAX_CYCLES DATA_MEM_BITS DATA_MEM_MIN_WAIT DATA_MEM_MAX_WAIT
require = $(foreach v,$(2),$(if $(strip $($(v))),,$(error $(1): $(v) missing)))
allow   = $(if $(filter $(3),$($(2))),,$(error $(1): $(2) = $($(2)), allowed: $(3)))
$(call require,$(CONFIG),$(REQ_CPU))
$(call require,fpga/platforms/$(PLATFORM)/$(PLATFORM).mk,$(REQ_PLATFORM))
$(call require,fpga/boards/$(BOARD)/board.mk,$(REQ_BOARD))
$(call require,tools/tools.mk,$(REQ_TOOLS))
$(call require,tb/tb.mk,$(REQ_TB))
$(call allow,$(CONFIG),RF_RAM_STYLE,auto distributed block registers)
$(call allow,$(CONFIG),IQ_RAM_STYLE,auto distributed block registers)
$(call allow,tools/tools.mk,IMPL_EFFORT,default performance)

# Vivado from tools/tools.mk, or found with locate
VIVADO_BIN = $(if $(filter auto,$(VIVADO)),$(shell locate bin/vivado | grep '/bin/vivado$$' | head -1),$(VIVADO))
XBIN       = $(dir $(VIVADO_BIN))

# Test: tb/tests/<suite>/<TEST>/ (.s or .c), all when empty
TEST ?=
FW   ?=
PRJ  ?=
ifneq ($(TEST),)
ifeq ($(wildcard tb/tests/*/$(TEST)),)
$(error Test $(TEST) not found in tb/tests/*/)
endif
endif

# Domain and action from the command line
DOMAINS = cpu fpga
ACTIONS = sim synth bitstream fw-update
DOMAIN  = $(firstword $(filter $(DOMAINS),$(MAKECMDGOALS)))
ACTION  = $(firstword $(filter $(ACTIONS),$(MAKECMDGOALS)))

.PHONY: help check $(DOMAINS) $(ACTIONS) cpu-sim cpu-sim-np cpu-sim-prj cpu-synth fpga-sim fpga-bitstream \
	fpga-fw-update fw-build clear-results ip impl sweep res clean FORCE

# Print commands and options
help:
	@echo "CPU (TEST=<name>: one test from tb/tests/*/<name>/, default all):"
	@echo "  make cpu sim       <TEST=...> <PRJ=1>      regression in xsim, PRJ=1 through a Vivado project"
	@echo "  make cpu synth                             out-of-context synthesis and implementation"
	@echo ""
	@echo "FPGA (PLATFORM=$(PLATFORM) BOARD=$(BOARD), FW=<name> from sw/fw/<platform>/, default $(DEFAULT_FW)):"
	@echo "  make fpga sim      <TEST=...>              regression on the platform block design"
	@echo "  make fpga sim      FW=<name>               firmware run for the waveform"
	@echo "  make fpga bitstream <FW=...>               bitstream with the firmware"
	@echo "  make fpga fw-update FW=<name>              firmware into the built bitstream"
	@echo ""
	@echo "  make check                                 tools and configuration"
	@echo "  make res                                   status of all runs"
	@echo "  make clean                                 remove out/"

# Tools present and usable, configuration already checked when parsing
check:
	@ok=1; \
	[ -x "$(VIVADO_BIN)" ] || { echo "tools/tools.mk: Vivado not found (VIVADO = $(VIVADO))"; ok=0; }; \
	for t in $(CROSS)-gcc $(CROSS)-as $(CROSS)-objdump $(CROSS)-objcopy gcc perl od truncate; do \
		command -v $$t > /dev/null || { echo "tools/tools.mk: $$t not found"; ok=0; }; \
	done; \
	[ -f "$$($(CROSS)-gcc -march=rv32i -mabi=ilp32 -print-libgcc-file-name 2> /dev/null)" ] || \
		{ echo "tools/tools.mk: $(CROSS)-gcc has no rv32i/ilp32 libgcc"; ok=0; }; \
	$(PYTHON) -c "import playwright" 2> /dev/null || { echo "tools/tools.mk: $(PYTHON) has no playwright"; ok=0; }; \
	[ $$ok = 1 ]

# Dispatch: make cpu sim -> cpu-sim
$(DOMAINS): check
	@$(MAKE) --no-print-directory $(if $(ACTION),$@-$(ACTION),help)

$(ACTIONS):
	@$(if $(DOMAIN),:,echo "Use: make cpu $@ or make fpga $@"; exit 1)

cpu-bitstream cpu-fw-update fpga-synth:
	@echo "No $(subst -, ,$@), see make help"; exit 1

# Remove old test results
clear-results:
	@rm -f $(RES_DIR)/$(or $(TEST),*)/comparison_results.txt $(RES_DIR)/$(or $(TEST),*)/ipc.txt

# CPU simulation, non-project snapshot
NP_DIR      = $(CPU_SIM_OUT)/sim_np
NP_SNAPSHOT = tb_np
NP_STAMP    = $(NP_DIR)/.$(NP_SNAPSHOT).stamp
NP_PROGRAM  = $(CURDIR)/$(TEST_DIR)/instr_mem.txt
NP_DATA     = $(CURDIR)/$(TEST_DIR)/data_mem.txt
NP_GENERICS = $(foreach g,$(CORE_GENERICS) $(TB_GENERICS),-generic_top "$(g)=$($(g))")

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
	tb/cpu/axi_lite_mem_model.vhd \
	tb/cpu/riscv_top_tb.vhd

# Generics of the last snapshot, a new value rebuilds it
$(NP_DIR)/config.used: FORCE
	@mkdir -p $(NP_DIR)
	@echo '$(NP_GENERICS)' | cmp -s - $@ || echo '$(NP_GENERICS)' > $@

# Build simulation snapshot
$(NP_STAMP): $(NP_SRCS) $(NP_DIR)/config.used
	@echo "Building simulation snapshot (non-project)"
	@mkdir -p $(NP_DIR) $(TEST_DIR)
	@cd $(NP_DIR) && $(XBIN)xvhdl -2008 $(addprefix $(CURDIR)/,$(NP_SRCS)) > /dev/null || { grep -E 'ERROR' xvhdl.log; exit 1; }
	@cd $(NP_DIR) && $(XBIN)xelab -debug typical $(NP_GENERICS) -generic_top "PROGRAM_FILE=$(NP_PROGRAM)" -generic_top "DATA_FILE=$(NP_DATA)" work.riscv_top_tb -s $(NP_SNAPSHOT) > /dev/null || { grep -E 'ERROR' xelab.log; exit 1; }
	@touch $@

# CPU regression, xsim or Vivado project
cpu-sim: RES_DIR = $(CPU_SIM_OUT)
cpu-sim: $(if $(PRJ),cpu-sim-prj,cpu-sim-np)
	@./$(TB_SCRIPTS)/verdict.sh "$(TEST)" $(CPU_SIM_OUT)

cpu-sim-np: RES_DIR = $(CPU_SIM_OUT)
cpu-sim-np: $(NP_STAMP) clear-results
	@touch $(TEST_DIR)/instr_mem.txt $(TEST_DIR)/data_mem.txt
	@START_TIME=$$(date +%s); \
	cd $(NP_DIR) && TEST=$(TEST) $(XBIN)xsim $(NP_SNAPSHOT) -tclbatch $(CURDIR)/$(TB_TCL)/run_np.tcl; \
	ELAPSED=$$(( $$(date +%s) - START_TIME )); \
	echo "Simulation time: $$((ELAPSED / 60)) min $$((ELAPSED % 60)) s"

cpu-sim-prj: RES_DIR = $(CPU_SIM_OUT)
cpu-sim-prj: clear-results
	@mkdir -p $(CPU_SIM_OUT)
	@START_TIME=$$(date +%s); \
	$(VIVADO_BIN) -mode batch -journal $(CPU_SIM_OUT)/vivado.jou -log $(CPU_SIM_OUT)/vivado.log -source $(VIVADO_SCRIPT) -tclargs $(or $(TEST),regression); \
	ELAPSED=$$(( $$(date +%s) - START_TIME )); \
	echo "Simulation time: $$((ELAPSED / 60)) min $$((ELAPSED % 60)) s"

# CPU out-of-context synthesis and implementation; any program, an empty memory would be optimized away
cpu-synth:
	@mkdir -p $(CPU_OUT)/synth
	@./$(TB_SCRIPTS)/prepare.sh full_test cpu > /dev/null
	@START_TIME=$$(date +%s); \
	$(VIVADO_BIN) -mode batch -nojournal -log $(CPU_OUT)/synth/vivado.log -source $(TCL_DIR)/synth_ooc.tcl -tclargs $(CURDIR)/$(TEST_DIR)/instr_mem.txt > /dev/null || { grep -E '^ERROR' $(CPU_OUT)/synth/vivado.log; exit 1; }; \
	echo "Synthesis and implementation done in $$(( $$(date +%s) - START_TIME )) s"
	@python3 RV32I/scripts/critical_paths.py $(CPU_OUT)/synth/paths.txt > $(CPU_OUT)/synth/critical_paths.txt
	@head -11 $(CPU_OUT)/synth/critical_paths.txt

# CPU IP packaging (needed by the platform), again only when the CPU RTL changes
CPU_RTL = $(filter RV32I/%,$(NP_SRCS))
IP_XML  = $(CPU_OUT)/ip_repo/CPU/component.xml
ip: $(IP_XML)
$(IP_XML): $(CPU_RTL) $(TCL_DIR)/package_ip.tcl
	@mkdir -p $(CPU_OUT)
	@START_TIME=$$(date +%s); \
	$(VIVADO_BIN) -mode batch -nojournal -log $(CPU_OUT)/ip_package.log -source $(TCL_DIR)/package_ip.tcl > /dev/null || { grep -E '^ERROR' $(CPU_OUT)/ip_package.log; exit 1; }; \
	echo "IP packaged in $$(( $$(date +%s) - START_TIME )) s"

# Firmware for the platform, default when FW is missing
fw-build:
	@mkdir -p $(FW_OUT)
	@fw=$(or $(FW),$(DEFAULT_FW)); \
	if [ ! -f sw/fw/$(PLATFORM)/$$fw.c ]; then \
		echo "FW $$fw not found in sw/fw/$(PLATFORM), using $(DEFAULT_FW)"; fw=$(DEFAULT_FW); \
	fi; \
	sw/common/build.sh sw/fw/$(PLATFORM)/$$fw.c $(FW_OUT)/$$fw fpga/platforms/$(PLATFORM)/$(PLATFORM).mk && \
	echo $$fw > $(FW_OUT)/selected

# Platform regression, or firmware run with FW: plain xsim on the last snapshot when its inputs are unchanged
BD_XSIM = $(BD_SIM_OUT)/sim/sim.sim/sim_1/behav/xsim
BD_KEY  = $(BD_SIM_OUT)/snapshot.key
BD_DEPS = $(CPU_RTL) $(wildcard fpga/rtl/*.vhd) fpga/platforms/$(PLATFORM)/platform_bd.tcl \
          tb/fpga/$(PLATFORM)/platform_tb.vhd $(TB_TCL)/sim_bd.tcl
BD_CFG  = $(foreach v,$(CFG_NAMES),$(v)=$($(v)))
fpga-sim: RES_DIR = $(BD_SIM_OUT)
fpga-sim: $(IP_XML) $(if $(FW),fw-build,clear-results)
	@mkdir -p $(BD_SIM_OUT)
	@START_TIME=$$(date +%s); \
	export TEST=$(TEST) FW_DIR=$(if $(FW),$(CURDIR)/$(FW_OUT)/$$(cat $(FW_OUT)/selected)); \
	key=$$( { cat $(BD_DEPS); echo '$(BD_CFG)'; } | md5sum | cut -d' ' -f1 ); \
	if [ -d $(BD_XSIM)/xsim.dir/platform_tb_behav ] && [ "$$key" = "$$(cat $(BD_KEY) 2> /dev/null)" ]; then \
		echo "Reusing the platform snapshot"; \
		( cd $(BD_XSIM) && $(XBIN)xsim platform_tb_behav -tclbatch $(CURDIR)/$(TB_TCL)/run_bd.tcl ) > $(BD_SIM_OUT)/sim_bd.log 2>&1 || { grep -E 'ERROR' $(BD_SIM_OUT)/sim_bd.log; exit 1; }; \
	else \
		rm -f $(BD_KEY); \
		$(VIVADO_BIN) -mode batch -nojournal -log $(BD_SIM_OUT)/sim_bd.log -source $(TB_TCL)/sim_bd.tcl > /dev/null || { grep -E '^ERROR' $(BD_SIM_OUT)/sim_bd.log; exit 1; }; \
		echo $$key > $(BD_KEY); \
	fi; \
	ELAPSED=$$(( $$(date +%s) - START_TIME )); \
	echo "Block design simulation time: $$((ELAPSED / 60)) min $$((ELAPSED % 60)) s"
	@$(if $(FW),grep -h '^Waveform' $(BD_SIM_OUT)/sim_bd.log,./$(TB_SCRIPTS)/verdict.sh "$(TEST)" $(BD_SIM_OUT))

# Platform bitstream with the firmware
BIT_DIR = $(FPGA_OUT)/bitstream
fpga-bitstream: $(IP_XML) fw-build
	@mkdir -p $(BIT_DIR)
	@fw=$$(cat $(FW_OUT)/selected); START_TIME=$$(date +%s); \
	$(VIVADO_BIN) -mode batch -nojournal -log $(BIT_DIR)/vivado.log -source $(FPGA_TCL)/bitstream.tcl \
		-tclargs $(CURDIR)/$(FW_OUT)/$$fw/instr_mem.txt $(CURDIR)/$(FW_OUT)/$$fw/data_mem.txt > /dev/null || { grep -E '^ERROR' $(BIT_DIR)/vivado.log; exit 1; }; \
	echo $$fw > $(BIT_DIR)/firmware; \
	echo "Bitstream with $$fw done in $$(( $$(date +%s) - START_TIME )) s"
	@python3 RV32I/scripts/critical_paths.py $(BIT_DIR)/paths.txt > $(BIT_DIR)/critical_paths.txt
	@head -11 $(BIT_DIR)/critical_paths.txt

# Firmware code and data into the built bitstream (no rebuild)
fpga-fw-update: fw-build
	@fw=$$(cat $(FW_OUT)/selected); dir=$(CURDIR)/$(FW_OUT)/$$fw; \
	data=""; [ -s $$dir/data_mem.txt ] && data="-data $$dir/$${fw}_dmem.elf -proc dmem"; \
	cd $(BIT_DIR) && $(XBIN)updatemem -force -meminfo mem.mmi -data $$dir/$${fw}_imem.elf -proc imem $$data \
		-bit design_1_wrapper.bit -out design_1_wrapper_$$fw.bit > /dev/null || { grep -E 'ERROR' updatemem.log; exit 1; }; \
	echo "Bitstream: $(BIT_DIR)/design_1_wrapper_$$fw.bit"

# Tools, see RV32I/README.md: place and route only, sweep
IMPL_DIR = $(FPGA_OUT)/impl/$(PLACE_DIRECTIVE)_$(ROUTE_DIRECTIVE)
impl:
	@mkdir -p $(IMPL_DIR)
	@START_TIME=$$(date +%s); \
	$(VIVADO_BIN) -mode batch -nojournal -log $(IMPL_DIR)/vivado.log -source $(FPGA_TCL)/impl.tcl > /dev/null || { grep -E '^ERROR' $(IMPL_DIR)/vivado.log; exit 1; }; \
	echo "Implementation done in $$(( $$(date +%s) - START_TIME )) s"
	@python3 RV32I/scripts/critical_paths.py $(IMPL_DIR)/paths.txt > $(IMPL_DIR)/critical_paths.txt
	@head -4 $(IMPL_DIR)/critical_paths.txt

sweep:
	@python3 RV32I/scripts/sweep.py

# Status of all runs
res:
	@python3 $(TB_SCRIPTS)/status.py

# Clean generated files
clean:
	rm -rf out .Xil RV32I/scripts/__pycache__ tb/scripts/__pycache__
	rm -f vivado*.jou vivado*.log
	@echo "Clean finished"

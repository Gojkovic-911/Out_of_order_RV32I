# Out_of_order_RV32I

Out-of-order RV32I core in VHDL, Vivado-only flow, running on FPGA (Zybo, Zynq-7010, PL only, no PS7). Long-term goal: port Zephyr RTOS and put the board on the network. Zephyr needs, in order: Zicsr, M-mode traps (mstatus, mtvec, mepc, mcause, mtval, mie, mip, mscratch, ecall/ebreak/mret, precise exceptions at commit), machine timer (mtime/mtimecmp), UART console, unified instruction/data address space (loads must read .rodata), fence/fence.i, more memory, then a Zephyr SoC/board port. A SHA3 accelerator platform (fpga/platforms/sha3) is planned for Zephyr crypto/PSA later.

## Layout and where to find things

- `RV32I/` CPU only, `RV32I/README.md` lists the CPU components
  - `cpu.mk` CPU configuration: `CORE_GENERICS` (testbench, synthesis, IP), `SYNTH_GENERICS` (RF/IQ_RAM_STYLE, synthesis and IP), `SYNTH_CLK_MHZ` / `SYNTH_PART` for `make cpu synth` (`platform` / `board` take CPU_CLK_MHZ / PART of the selection); table in `RV32I/README.md`
  - `rtl/`
    - `riscv_top_mem.vhd` IP top: CPU + instruction BRAM (`INSTR_MEM`, INIT_FILE = program), AXI4-Lite data port `m_axi_*`
    - `riscv_top.vhd` core: `data_path_1`, `control_path_1`, `axi_bridge` (`axi_lite_master.vhd`)
    - `control_path/` decoders (`ctrl_decoder`, `alu_decoder`), `branch_predictor.vhd` (fetch predecode: branch/JAL taken, JALR flag), `control_path.vhd` (stall process, pc_next_sel on flush)
    - `data_path/data_path.vhd` pipeline glue and registers: `phys_regs_write`/`phys_regs_read` (physical register file), `fetch_addr` (next PC mux), `pc_reg`, `IF_ID`, `ID_RN`, `generate_rob_jump_addr` (branch fall-through PC into ROB), `jalr_redirect` (EX redirect), IS/EX register, instances below
    - `data_path/renaming_module.vhd` rename table, commit rename table, phys_ready bits, free list instance
    - `data_path/free_list_fifo.vhd` free physical registers
    - `data_path/ROB.vhd` reorder buffer, commit, mispredict flush, `rob_deadlock_check` (sim assert after 1000 stuck cycles)
    - `data_path/issue_module.vhd` IQ: status flip-flops `iq_ffs_s`, payload LUTRAM `iq_ram_s`, dispatch slot search, issue select, `payload_rd`
    - `data_path/ex_router.vhd` steering to ALU / branch unit / AGU+LSQ, ALU operand mux, `ex_busy_o`
    - `data_path/alu_unit.vhd` combinational ALU with CDB request/grant; `branch_unit.vhd` branch decision + registered outcome to ROB
    - `data_path/LSQ.vhd` load/store queue, `LSU.vhd` byte/half/word alignment and sign extension
    - `data_path/cdb.vhd` round-robin ALU/LSQ arbiter + CDB register
    - `packages/` instruction type/subtype constants (`instr_types_pkg.vhd`), ALU ops
  - `scripts/`
    - `tcl/config.tcl` sourced by every Vivado script: paths (CPU, tb, out), loads cpu.mk + <platform>.mk + board.mk + tools.mk + tb.mk into `cfg` (env `CFG_<name>` from make wins, so command line overrides reach Tcl), resolves SYNTH_CLK_MHZ/SYNTH_PART, `effort_args`, `write_paths`, `cfg_generics`
    - `tcl/synth_ooc.tcl`, `tcl/package_ip.tcl`, `critical_paths.py` groups timing paths, `sweep.py` size sweep
- `fpga/`
  - `scripts/bitstream.tcl` project, block design, implementation, reports, MMI; `scripts/impl.tcl` place and route only from the last platform synthesis (`make impl`); `scripts/mem_mmi.tcl` (`write_mem_mmi`: one MMI with `imem` and `dmem` processors, 4 byte-lane RAMB36 each)
  - `rtl/axi_lite_ram.vhd` data memory for every platform: AXI4-Lite slave, byte writes, registered read, generics ADDR_BITS, INIT_FILE, SIM_LOAD; module reference in the block design (no IP packaging), plain VHDL
  - `platforms/simple/` CPU + 16 KB data memory (`dmem_0`, axi_lite_ram) + 4-bit GPIO + clock wizard + proc_sys_reset
    - `<platform>.mk` (CPU_CLK_MHZ, DMEM_BASE, DMEM_SIZE, GPIO_BASE, DEFAULT_FW), `platform_bd.tcl` (`create_platform_bd <bd> <instr_mem.txt> <data_mem.txt> <sim_load>`, `mem_bits` for ADDR_BITS from DMEM_SIZE)
  - `platforms/sha3/` empty placeholder (`.gitkeep`) for the planned SHA3 accelerator platform
  - `boards/zybo/` `board.mk` (PART, BOARD_CLK_MHZ), `zybo.xdc` (clock L16, BTN0 reset R18, LD0-LD3)
- `tb/` everything that runs or checks a simulation, shared by the CPU and the platforms
  - `cpu/` `riscv_top_tb.vhd` CPU testbench (instruction BRAM, AXI-Lite memory model, cycle-accounting counters via VHDL-2008 external names, idle-fetch stop after 200 cycles of instruction 0), `axi_lite_mem_model.vhd` (random MIN..MAX wait per handshake, LFSR; generic INIT_FILE loaded during reset, tb generic DATA_FILE = `out/tb/tests/data_mem.txt`, empty for asm tests), `.wcfg`
  - `fpga/<platform>/platform_tb.vhd` platform testbench (generic BOARD_CLK_MHZ)
  - `tests/stress_tests/<name>/<name>.s` assembly stress tests (13, golden model), run by `make cpu sim` and `make fpga sim`
  - `tests/benchmark/<name>/*.c` self-checking C programs (result in a0 vs host run): `crc32` (uses lbu, `__mulsi3`)
  - `coverage/implemented_instructions.md` instruction table (first column read), `coverage/coverage.py` coverage report (room for a coverage tool later)
  - `scripts/` `prepare.sh <test> <cpu|platform>` stages any test (.s through compile.sh, .c through build.sh for the target map + host run), `check.sh <test> <results>` (golden registers or a0 vs host), `status.py` (`make res`), `check_app.py`, `compile.sh` (assembles into `out/tb/tests/instr_mem.txt` + `.dis`, runs coverage.py), `get_golden_vector.py` (Cornell web interpreter via playwright, cached by SHA-256 of the `.s`), `compare_regs.sh <test> [results dir]`, `verdict.sh [test] [results dir]`, `cycles_report.py [results dir]`
  - `scripts/tcl/` `run_np.tcl` (xsim cannot source config.tcl, computes its own paths), `RISCV.tcl` (project regression, CRLF file), `sim_bd.tcl` (platform regression; SIM_LOAD true, so per test only `restart`, both memories reread their files; `FW_DIR` env: one firmware run of `SIM_US` us with `log_wave -recursive /`), `tests.tcl` (`test_list`: TEST or every `tb/tests/*/<name>/`), `print_current_state.tcl` (register + IPC dump into `$results/<test>/`)
- `sw/` C build flow and firmware, built by the command that uses it (no separate make target)
  - `common/link.ld` shared sections (Harvard: `.text` in IMEM, `.rodata`/`.data`/`.bss`/stack in DMEM, both at 0, so `--no-check-sections`), `INCLUDE memory.ld`
  - `common/crt0.S` gp, sp, zero `.bss`, `call main`, then jump past the code into zero words (idle-fetch stop); result stays in a0
  - `common/build.sh <src dir|file> <out> [<platform>.mk] [--host]` (IMEM 2^WADDR, DMEM from <platform>.mk or 0..2^WADDR) generates `memory.ld` + `platform.h` (every `*_BASE` and `*_MHZ` from <platform>.mk), compiles with libgcc (no M extension), writes `instr_mem.txt` / `data_mem.txt` (one 32-bit binary word per line), `.dis`, `.map`, `<app>_imem.elf` (code only, for updatemem), and with `--host` `expected.txt` from a host run (`common/host_main.c`, app main renamed `app_main`)
  - `fw/<platform>/<name>.c` platform firmware: `simple/led_blink.c` (GPIO counter, delay from CPU_CLK_MHZ)
- `tools/` `tools.mk` (machine and tool settings), `README.md` with its table and the list of caches with run times
- `out/` everything generated, same layout as the repository (`make clean` removes it)
  - `out/RV32I/` `synth/`, `ip_repo/`, `ip_package/`, `sweep/`
  - `out/fpga/<platform>_<board>/` `project/`, `bitstream/` (+ `firmware` name, `data_mem.coe`), `impl/<place>_<route>/`
  - `out/sw/fw/<platform>/<fw>/` firmware build, `selected` = firmware actually used
  - `out/tb/tests/stress_tests/<test>/` program `.dis`, golden vector; `out/tb/tests/instr_mem.txt` + `data_mem.txt` last loaded program
  - `out/tb/coverage/` `coverage.md` summary + matrix, `<test>.md` per test
  - `out/tb/tests/benchmark/<test>/<target>/` benchmark build per memory map, `expected.txt` next to it
  - `out/tb/cpu/` `sim_np/`, `RISCV_project/`, `<test>/` (sim registers, `ipc.txt`, `comparison_results.txt`) for `make cpu sim`
  - `out/tb/fpga/<platform>_<board>/` `sim/` project, `sim_bd.log`, `<test>/` results for `make fpga sim`
- `Makefile` drives everything: `make cpu <sim|synth>`, `make fpga <sim|bitstream|fw-update>` (the domain word dispatches to `cpu-sim`, `fpga-bitstream`...; the action word alone is an error); `PLATFORM ?= simple`, `BOARD ?= zybo` (exported to Tcl as env, fpga only, though `cpu synth` still takes CPU_CLK_MHZ from the default <platform>.mk); `make` alone prints the command list
- `README.md` user documentation with a fixed structure and the TODO list

## Commands and run times (Vivado 2023.2, 16 threads, 15 GB)

- `make cpu sim` all tests in `tb/tests/*/` (~50 s with cached golden vectors; golden vectors need internet after `make clean`), `TEST=<name>` one test (.s or .c found by name), `PRJ=1` through a Vivado project (~3 min); C tests run up to 1M cycles (tb clock 100 ns)
- `make cpu synth` CPU out-of-context synthesis + implementation (~5 min) at SYNTH_CLK_MHZ on SYNTH_PART; full_test sits in the instruction memory only so the memory is not optimized away
- `make fpga sim` all tests on the platform block design: first run builds the Vivado sim project (~2-3 min, `sim_bd.tcl`), later runs with unchanged inputs (md5 of CPU RTL, fpga/rtl, platform_bd.tcl, platform tb, sim_bd.tcl and all config values, `snapshot.key`) run the snapshot in plain xsim (`run_bd.tcl`, ~50 s for 14 tests, ~10 s for FW); `make ip` repackages only when the CPU RTL changes, `make fpga sim FW=<name>` firmware run for the waveform (no check, `SIM_US`, default 200)
- `make fpga bitstream <FW=...>` (~15-26 min), firmware from `sw/fw/<platform>/`; missing or unknown FW falls back to DEFAULT_FW with a message
- `make fpga bitstream` uses the Vivado IP cache `out/fpga/ip_cache` for the Xilinx IPs (clk_wiz, gpio, proc_sys_reset, interconnect); cache entries of the CPU and dmem (our RTL) are deleted before each build, so they are always synthesized (BD IPs cannot be excluded with generate_synth_checkpoint, read-only). Saves ~1.5 min
- `make fpga fw-update FW=<name>` code (`<fw>_imem.elf`, proc imem) and, when present, data (`<fw>_dmem.elf`, proc dmem) into the built bitstream with updatemem and `bitstream/mem.mmi` (~17 s)
- `make res` status of all runs (tests PASS count, synth/bitstream WNS/WHS/LUT, firmware); `python3 tb/scripts/cycles_report.py` cycle accounting
- `ip` package the CPU IP (dependency of fpga targets); `make impl` place and route only from the last bitstream synthesis (sweep tool, ~17 min); `make sweep` and `sweep.py --directives` documented in RV32I/README.md, report in `out/RV32I/sweep/results.md`
- `make check` tools present (Vivado, toolchain + rv32i libgcc, gcc, perl, python with playwright); runs before every `make cpu|fpga ...`. At parse time make stops on a missing (REQ_* lists) or invalid (RAM styles, IMPL_EFFORT) value, naming the file
- Configuration files: `RV32I/cpu.mk`, `fpga/platforms/<p>/<platform>.mk` (+ GPIO_WIDTH), `fpga/boards/<b>/board.mk`, `tools/tools.mk` (VIVADO, CROSS, PYTHON, CFLAGS, MAX_JOBS, IMPL_EFFORT, all step directives), `tb/tb.mk` (TB_GENERICS for riscv_top_tb, MAX_CYCLES); each described in the README next to it (`tools/README.md` also lists the caches); any value can be given on the command line
- `make clean` removes `out/`, `.Xil`, Vivado logs
- `CONFIG=<file>` uses another CPU config (sweeps, experiments); `VIVADO=<path>` if `locate` does not find Vivado
- Never run two Vivado jobs at the same time (machine runs out of memory). Never `make clean` while a build runs (Vivado loses its working directory). When stopping Vivado, kill by PID; `pkill -f` with a pattern that also matches your own shell command kills the shell.

## Architecture

- Pipeline: fetch, decode (IF/ID), rename (ID/RN), dispatch into IQ + ROB (+ LSQ for memory ops), issue select, issue read (payload register + physical register file read into IS/EX), execute, CDB, commit.
- Fetch: static always-taken branch prediction; branch and JAL redirect in decode (`decode_pc + imm`, one bubble). JALR: fetch stalls while JALR is in decode and until EX (`jalr_wait_s`), then a registered one-cycle redirect from EX (`jalr_redirect_s`, target `(rs1 + imm) & ~1`); JALR is not a ROB branch, no commit flush.
- Rename: speculative `rename_table_s` and `commit_rename_table_s` in flip-flops (flush copies the whole commit table in one cycle, RAM cannot); sources are read before rd is renamed. Free list: ring of 63 slots (register 0 never free) with `wr_ptr`, speculative `rd_ptr`, `rd_ptr_commit` and counters; push of the previous mapping at commit; flush sets `rd_ptr <= rd_ptr_commit`. Memory in flip-flops because reset reloads 1..63.
- phys_ready bits: cleared on rename, set by CDB, all set on flush (committed state is ready). CDB bypass at rename and in the rename-to-IQ register so a broadcast in the same cycle is not lost.
- ROB: ring with head/tail and per-entry valid bit; empty/full both have head = tail and differ by `rob(head).valid`. Payload in two LUTRAMs (rename word, branch address). Nop (`0x00000013`) enters ready. Commit: rd/prev phys to rename + free list, store commit to LSQ; branch with `branch_taken = 0` (meaning prediction wrong) flushes everything, `tail <= head + 1`, fetch restarts from the stored fall-through PC.
- IQ: status flip-flops (valid, rs1/rs2 addr, ready) because CDB wakeup compares all entries every cycle; payload (rd, type, subtype, imm, pc, rob_idx, lsq_idx, instruction) in LUTRAM as flat `std_logic_vector` words (Vivado does not infer RAM from record arrays). Issue select picks only entries with both operands ready (cycle N, select -> async LUTRAM read -> `payload_rd` register, plus registered rs1/rs2 addresses); cycle N+1 reads the physical register file into IS/EX. Dispatch skips the entry at `issue_addr_reg_s`.
- No CDB bypass at the register file read: a value broadcast in cycle C is written to the register file at the end of C and the earliest reader reads it in C+2.
- Execute: router sends ALU types (incl. LUI, AUIPC, JAL, JALR rd = pc+4) to the ALU, branches to the branch unit, loads/stores to the AGU (rs1 + imm) and LSQ fill. `ex_busy` (ALU not granted the CDB) stalls IS/EX and the IQ issue; unit valids do not depend on the stall (no combinational loop).
- LSQ: program-order queue allocated at rename; memory accesses only from the head; stores after ROB commit, loads speculative (response discarded on flush); a load behind a store waits for it.
- Stalls (`control_path.vhd` stall process): ROB full, free list empty, LSQ full and IQ full stall fetch/decode/rename; this IQ stall chain is the critical timing path. Backpressure (valid/ready) is a configurable TODO.
- Data memory: AXI4-Lite master; in the platform an AXI BRAM controller (fixed latency), in the CPU testbench the random-wait model.

## Vivado and flow details

- Physical register file: `RF_RAM_STYLE = auto` puts it in two RAMB18 (one per read port) because the read output now feeds IS/EX directly; the IS/EX data FFs become the BRAM output latch (BRAM clock-to-out ~2 ns moves into EX). `distributed` keeps it in LUTRAM (RAM64M x 22). Same timing at 120 MHz OOC, BRAM saves ~250 LUT.
- VHDL is case-insensitive: a generic named `RAM_STYLE` hides the attribute `ram_style` (the IQ generic is `PAYLOAD_RAM_STYLE`). Attribute values from generics work in Vivado.
- Python: golden vectors run through the system `/usr/bin/python3` (playwright installed there); Vivado sets PYTHONHOME/PYTHONPATH, so `run_np.tcl`, `sim_bd.tcl` and `RISCV.tcl` unset them before `exec`.
- Memory init: IMEM (CPU IP generic INIT_FILE, `BRAM_byte_addressable.vhd`) and DMEM (`axi_lite_ram`) both get their initial value from a textio function at elaboration; synthesis turns it into BRAM INIT bits, updatemem patches them later. SIM_LOAD (simulation only, inside `pragma translate_off`) also reloads the file on the first clock, so the platform simulation changes programs with `restart` instead of a new elaboration. The CPU testbench loads both memories with processes during reset.
- blk_mem_gen behind axi_bram_ctrl (BRAM_Controller mode) keeps Load_Init_File/Coe_File disabled in the block design (ignored at create, after address assignment and after validation); that is why the data memory is our own RTL.
- updatemem MMI: lanes must be listed from the lowest byte; updatemem assigns bytes by BitLane order, not DataWidth. `ram_s_reg_N` holds byte N; MemType RAMB32; LOC without the RAMB36_ prefix. Verified: same program gives an identical bitstream.
- Platform simulation: external names do not cross the VHDL/Verilog wrapper (XSIM 43-4620), so `sim_bd.tcl` stops on idle fetch in Tcl (`run_program`, 5 us of zero fetch); `INCREMENTAL false` so INIT_FILE is re-read for every test.
- `set_property STEPS.SYNTH_DESIGN.ARGS.GLOBAL_RETIMING on` (the old RETIMING property is deprecated). DRC ZPS7-1 (no PS7) is benign for a JTAG-loaded PL-only design.
- Timing: target 120 MHz (CPU_CLK_MHZ in <platform>.mk). Critical paths start at `iq_ffs.valid` / `dispatch_ptr` (IQ full -> stall chain -> phys_ready, rename table, instruction memory address). Place directive is `PLACE_DIRECTIVE = ExtraNetDelay_high`, route `AggressiveExplore`; an earlier sweep at 120 MHz found AltSpreadLogic_high best (+0.168 ns before the latest RTL changes). `RV32I/scripts/directive_sweep.sh` (untracked) is superseded by `sweep.py --directives`.
- IPC is ~0.25 regardless of ROB/IQ/LSQ sizes: limited by a 4-cycle dependency loop (select, read, execute, CDB/wakeup); the smallest configuration gives the best MIPS.
- Golden model limits: Cornell interpreter, final register state only, no `lbu` support; lh/lbu/lhu/sh are not covered by any test yet. Spike golden model is a TODO.

## Status (update as the project moves)

- 2026-10-31: flow work finished and committed: layout (RV32I/, fpga/, tb/, sw/, tools/, single out/), `make cpu|fpga` commands, split configuration with checks, C flow on the CPU and the platform (dmem init, updatemem for both memories), caches (listed in tools/). All targets pass: cpu sim 14/14, fpga sim 14/14, cpu synth WNS +0.254 ns, bitstream WNS +0.164 ns at 120 MHz (3 176 LUT).
- Default configuration ROB 32 / IQ 8 / LSQ 8 (size sweep 2026-10-30: IPC barely depends on sizes, IQ 16 has no 120 MHz margin; with 64 physical registers ROB above 32 rarely fills).
- Next phase is CPU design; flow changes only when the design needs them. Open design topics, roughly by value:
  - IPC ~0.3 bound by the 4-cycle dependency loop (select, read, execute, CDB/wakeup); a wakeup/bypass shortcut is the biggest lever
  - branch prediction: always-taken costs 18% refill on crc32 (34 982 cycles, IPC 0.41); JALR stalls fetch until EX
  - M extension (multiplier), then the Zephyr list above (Zicsr, traps, timer, UART)
  - critical path: IQ full -> stall chain (`iq_ffs.valid` / `dispatch_ptr`); backpressure TODO
  - known bug: memory waits 3..3, 4..4 or 0..8 fail six tests (call, compare, flush_mem, loop, memory, stall); not debugged
  - verification: Spike golden model (Cornell has no lbu; lh/lbu/lhu/sh uncovered), C benchmarks (Embench/CoreMark)
- Measure every design change with `make cpu sim` (IPC per test, `cycles_report.py`) and `make cpu synth` (WNS), then `make fpga bitstream` before claiming timing.

## Working rules

- Keep this file current: update it in the same change whenever layout, flow, commands, configuration or architecture change.
- Propose changes first (what, where, how); edit only after the user says to. Explain before optimizing.
- Commit and push straight to `main`, never create branches; no Co-Authored-By line; short English commit messages.
- Comments: at most one short comment (3-5 words) per 10+ lines, architecture/steps only; config files, Makefile targets and xdc blocks get a comment each.
- Readme files are locked (`.claude/hooks/readme_lock.py`): Claude changes only numbers, the user writes all text.
- When mentioning a file, give a clickable link with the line number.
- The user writes in Serbian/Croatian; answer in the same language. Repository content is in English.

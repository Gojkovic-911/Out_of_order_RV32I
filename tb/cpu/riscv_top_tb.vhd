library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;
use work.txt_util.all;
use work.instr_types_pkg.all;

library std;
use std.env.all;

entity riscv_top_tb is
generic (
        Vivado_version    : std_logic := '0';
        -- Core configuration generics
        DATA_WIDTH        : positive  := 32;
        MEM_BYTES         : positive  := 4;
        ARCH_ADDR_BITS    : positive  := 5;
        NUM_PHYS_REGS     : positive  := 64;
        ROB_DEPTH         : positive  := 64;
        IQ_DEPTH          : positive  := 16;
        LSQ_DEPTH         : positive  := 16;
        IMEM_ADDR_BITS    : natural   := 14;
        DMEM_BASE         : natural   := 0;
        DMEM_ADDR_BITS    : natural   := 14;
        DMEM_RD_LATENCY   : positive  := 1;
        -- Testbench generics (tb/tb.mk)
        TB_CLK_NS         : positive  := 100;
        IDLE_STOP_CYCLES  : positive  := 200;
        DATA_MEM_MIN_WAIT : natural   := 0;
        DATA_MEM_MAX_WAIT : natural   := 3;
        -- Program file to run
        PROGRAM_FILE      : string    := "../../../../../tests/instr_mem.txt";
        -- Data memory contents, empty: all zeros
        DATA_FILE         : string    := ""
    );
end entity;

architecture Behavioral of riscv_top_tb is
   signal clk                          : std_logic := '0';
   signal reset                        : std_logic;
   -- Cycle accounting, snapshot at last commit
   constant N_CNT : natural := 11;
   type cnt_t is array (0 to N_CNT-1) of natural;
   signal cnt_s, cnt_at_commit_s : cnt_t := (others => 0);
   -- m_axi requests (no slave in this testbench)
   signal awvalid_s, arvalid_s         : std_logic;

begin

   dut : entity work.riscv_top_mem
      generic map (
         DATA_WIDTH      => DATA_WIDTH,
         MEM_BYTES       => MEM_BYTES,
         ARCH_ADDR_BITS  => ARCH_ADDR_BITS,
         NUM_PHYS_REGS   => NUM_PHYS_REGS,
         ROB_DEPTH       => ROB_DEPTH,
         IQ_DEPTH        => IQ_DEPTH,
         LSQ_DEPTH       => LSQ_DEPTH,
         IMEM_ADDR_BITS  => IMEM_ADDR_BITS,
         DMEM_BASE       => DMEM_BASE,
         DMEM_ADDR_BITS  => DMEM_ADDR_BITS,
         DMEM_RD_LATENCY => DMEM_RD_LATENCY,
         DATA_MEM_MIN_WAIT => DATA_MEM_MIN_WAIT,
         DATA_MEM_MAX_WAIT => DATA_MEM_MAX_WAIT,
         IMEM_INIT_FILE  => PROGRAM_FILE,
         DMEM_INIT_FILE  => DATA_FILE,
         SIM_LOAD        => true)
      port map (
         clk   => clk,
         reset => reset,
         s_axi_aresetn => reset,
         -- Port B unused in the CPU regression
         s_axi_imem_awaddr  => (others => '0'),
         s_axi_imem_awlen   => (others => '0'),
         s_axi_imem_awsize  => (others => '0'),
         s_axi_imem_awburst => (others => '0'),
         s_axi_imem_awvalid => '0',
         s_axi_imem_wdata   => (others => '0'),
         s_axi_imem_wstrb   => (others => '0'),
         s_axi_imem_wlast   => '0',
         s_axi_imem_wvalid  => '0',
         s_axi_imem_bready  => '0',
         s_axi_imem_araddr  => (others => '0'),
         s_axi_imem_arlen   => (others => '0'),
         s_axi_imem_arsize  => (others => '0'),
         s_axi_imem_arburst => (others => '0'),
         s_axi_imem_arvalid => '0',
         s_axi_imem_rready  => '0',
         s_axi_dmem_awaddr  => (others => '0'),
         s_axi_dmem_awlen   => (others => '0'),
         s_axi_dmem_awsize  => (others => '0'),
         s_axi_dmem_awburst => (others => '0'),
         s_axi_dmem_awvalid => '0',
         s_axi_dmem_wdata   => (others => '0'),
         s_axi_dmem_wstrb   => (others => '0'),
         s_axi_dmem_wlast   => '0',
         s_axi_dmem_wvalid  => '0',
         s_axi_dmem_bready  => '0',
         s_axi_dmem_araddr  => (others => '0'),
         s_axi_dmem_arlen   => (others => '0'),
         s_axi_dmem_arsize  => (others => '0'),
         s_axi_dmem_arburst => (others => '0'),
         s_axi_dmem_arvalid => '0',
         s_axi_dmem_rready  => '0',

         m_axi_awaddr  => open,
         m_axi_awprot  => open,
         m_axi_awvalid => awvalid_s,
         m_axi_awready => '0',
         m_axi_wdata   => open,
         m_axi_wstrb   => open,
         m_axi_wvalid  => open,
         m_axi_wready  => '0',
         m_axi_bresp   => "00",
         m_axi_bvalid  => '0',
         m_axi_bready  => open,
         m_axi_araddr  => open,
         m_axi_arprot  => open,
         m_axi_arvalid => arvalid_s,
         m_axi_arready => '0',
         m_axi_rdata   => (others => '0'),
         m_axi_rresp   => "00",
         m_axi_rvalid  => '0',
         m_axi_rready  => open);

   -- Tests stay inside DMEM; m_axi has no slave here
   no_m_axi : process(clk)
   begin
      if rising_edge(clk) then
         assert reset /= '1' or (awvalid_s /= '1' and arvalid_s /= '1')
            report "Access outside DMEM on m_axi (no slave in the CPU testbench)" severity failure;
      end if;
   end process;

   -- Reset while the memories load their files
   reset_proc : process
   begin
      reset <= '0';
      for i in 1 to 4 loop
         wait until rising_edge(clk);
      end loop;
      reset <= '1' after 20 ns;
      wait;
   end process;

   -- Clock generator
   clk_proc : process
   begin
      clk <= '1', '0' after (TB_CLK_NS * 1 ns) / 2;
      wait for TB_CLK_NS * 1 ns;
   end process;

   -- Counters: 0 cycles, 1 commits, 2 rob_empty, 3 refill, 4 mispredicts, 5 jalr,
   -- 6 rob_full, 7 freelist, 8 lsq_full, 9 iq_full, 10 ex_busy
   cycle_count: process(clk)
      alias commit_valid is <<signal .riscv_top_tb.dut.CPU.data_path_1.rob_commit_valid_s : std_logic>>;
      alias rob_empty    is <<signal .riscv_top_tb.dut.CPU.data_path_1.rob_empty_s : std_logic>>;
      alias ex_valid     is <<signal .riscv_top_tb.dut.CPU.data_path_1.ex_valid_s : std_logic>>;
      alias ex_type      is <<signal .riscv_top_tb.dut.CPU.data_path_1.execute_instr_type_s : std_logic_vector(3 downto 0)>>;
      alias flush        is <<signal .riscv_top_tb.dut.CPU.flush_pipe_s : std_logic>>;
      alias stall_rn     is <<signal .riscv_top_tb.dut.CPU.stall_rn_s : std_logic>>;
      alias rob_full     is <<signal .riscv_top_tb.dut.CPU.rob_full_s : std_logic>>;
      alias fl_empty     is <<signal .riscv_top_tb.dut.CPU.free_list_fifo_empty_s : std_logic>>;
      alias lsq_full     is <<signal .riscv_top_tb.dut.CPU.lsq_full_s : std_logic>>;
      alias iq_full      is <<signal .riscv_top_tb.dut.CPU.iq_full_s : std_logic>>;
      alias ex_busy      is <<signal .riscv_top_tb.dut.CPU.ex_busy_s : std_logic>>;
      variable c      : cnt_t := (others => 0);
      variable refill : boolean := false;
      procedure inc(i : natural; cond : boolean) is
      begin
         if cond then
            c(i) := c(i) + 1;
         end if;
      end procedure;
   begin
      if rising_edge(clk) and reset = '1' then
         if flush = '1' then
            refill := true;
         elsif rob_empty = '0' then
            refill := false;
         end if;
         inc(0, true);
         inc(1, commit_valid = '1');
         inc(2, rob_empty = '1');
         inc(3, rob_empty = '1' and refill);
         inc(4, flush = '1');
         inc(5, ex_valid = '1' and ex_type = JALR);
         inc(6, rob_full = '1');
         inc(7, fl_empty = '1' and stall_rn = '1');
         inc(8, lsq_full = '1');
         inc(9, iq_full = '1');
         inc(10, ex_busy = '1');
         cnt_s <= c;
         if commit_valid = '1' then
            cnt_at_commit_s <= c;
         end if;
      end if;
   end process;

   -- Stop on idle fetch
   process(clk)
      alias instr is <<signal .riscv_top_tb.dut.instr_rdata : std_logic_vector(31 downto 0)>>;
      variable cnt : integer := 0;
   begin
      if rising_edge(clk) then
         if instr = std_logic_vector(to_unsigned(0, 32)) then
               cnt := cnt + 1;
               if cnt > IDLE_STOP_CYCLES then
                  std.env.stop;
               end if;
         else
               cnt := 0;
         end if;
      end if;
   end process;

end architecture;

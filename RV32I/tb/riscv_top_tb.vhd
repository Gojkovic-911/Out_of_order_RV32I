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
        WADDR             : natural   := 14;
        DATA_MEM_MAX_WAIT : natural   := 3;
        -- Program file to run
        PROGRAM_FILE      : string    := "../../../../../tests/assembly_code_bin.txt"
    );
end entity;

architecture Behavioral of riscv_top_tb is
   file RISCV_instructions             : text open read_mode is PROGRAM_FILE;
   signal clk                          : std_logic := '0';
   signal reset                        : std_logic;
   -- Instruction memory signals
   signal ena_instr_s, enb_instr_s     : std_logic;
   signal wea_instr_s, web_instr_s     : std_logic_vector(3 downto 0);
   signal addra_instr_s, addrb_instr_s : std_logic_vector(WADDR-1 downto 0);
   signal dina_instr_s, dinb_instr_s   : std_logic_vector(31 downto 0);
   signal douta_instr_s, doutb_instr_s : std_logic_vector(31 downto 0);
   signal addrb_instr_32_s             : std_logic_vector(31 downto 0);
   -- Cycle accounting, snapshot at last commit
   constant N_CNT : natural := 11;
   type cnt_t is array (0 to N_CNT-1) of natural;
   signal cnt_s, cnt_at_commit_s : cnt_t := (others => 0);
   -- AXI4-Lite data bus
   signal awaddr_s, araddr_s           : std_logic_vector(31 downto 0);
   signal awprot_s, arprot_s           : std_logic_vector(2 downto 0);
   signal awvalid_s, awready_s         : std_logic;
   signal wdata_s, rdata_s             : std_logic_vector(31 downto 0);
   signal wstrb_s                      : std_logic_vector(3 downto 0);
   signal wvalid_s, wready_s           : std_logic;
   signal bresp_s, rresp_s             : std_logic_vector(1 downto 0);
   signal bvalid_s, bready_s           : std_logic;
   signal arvalid_s, arready_s         : std_logic;
   signal rvalid_s, rready_s           : std_logic;

begin

   -- Instruction memory, init and fetch
   ena_instr_s   <= '1';
   enb_instr_s   <= '1';
   addrb_instr_s <= addrb_instr_32_s(WADDR-1 downto 0);
   web_instr_s   <= (others => '0');
   dinb_instr_s  <= (others => '0');
   instruction_mem : entity work.BRAM(behavioral)
      generic map(WADDR => WADDR)
      port map (clk      => clk,
                en_a_i   => ena_instr_s,
                we_a_i   => wea_instr_s,
                addr_a_i => addra_instr_s,
                data_a_i => dina_instr_s,
                data_a_o => douta_instr_s,
                en_b_i   => enb_instr_s,
                we_b_i   => web_instr_s,
                addr_b_i => addrb_instr_s,
                data_b_i => dinb_instr_s,
                data_b_o => doutb_instr_s);

   -- AXI-Lite data memory
   data_mem : entity work.axi_lite_mem_model
      generic map(WADDR => WADDR, MAX_WAIT => DATA_MEM_MAX_WAIT)
      port map (clk           => clk,
                reset         => reset,
                s_axi_awaddr  => awaddr_s,
                s_axi_awvalid => awvalid_s,
                s_axi_awready => awready_s,
                s_axi_wdata   => wdata_s,
                s_axi_wstrb   => wstrb_s,
                s_axi_wvalid  => wvalid_s,
                s_axi_wready  => wready_s,
                s_axi_bresp   => bresp_s,
                s_axi_bvalid  => bvalid_s,
                s_axi_bready  => bready_s,
                s_axi_araddr  => araddr_s,
                s_axi_arvalid => arvalid_s,
                s_axi_arready => arready_s,
                s_axi_rdata   => rdata_s,
                s_axi_rresp   => rresp_s,
                s_axi_rvalid  => rvalid_s,
                s_axi_rready  => rready_s);

   -- Processor core
   TOP_RISCV_1 : entity work.riscv_top
      generic map (
         DATA_WIDTH     => DATA_WIDTH,
         MEM_BYTES      => MEM_BYTES,
         ARCH_ADDR_BITS => ARCH_ADDR_BITS,
         NUM_PHYS_REGS  => NUM_PHYS_REGS,
         ROB_DEPTH      => ROB_DEPTH,
         IQ_DEPTH       => IQ_DEPTH,
         LSQ_DEPTH      => LSQ_DEPTH)
      port map (
         clk   => clk,
         reset => reset,

         instr_mem_data_i   => doutb_instr_s,
         instr_mem_addr_o   => addrb_instr_32_s,

         m_axi_awaddr  => awaddr_s,
         m_axi_awprot  => awprot_s,
         m_axi_awvalid => awvalid_s,
         m_axi_awready => awready_s,
         m_axi_wdata   => wdata_s,
         m_axi_wstrb   => wstrb_s,
         m_axi_wvalid  => wvalid_s,
         m_axi_wready  => wready_s,
         m_axi_bresp   => bresp_s,
         m_axi_bvalid  => bvalid_s,
         m_axi_bready  => bready_s,
         m_axi_araddr  => araddr_s,
         m_axi_arprot  => arprot_s,
         m_axi_arvalid => arvalid_s,
         m_axi_arready => arready_s,
         m_axi_rdata   => rdata_s,
         m_axi_rresp   => rresp_s,
         m_axi_rvalid  => rvalid_s,
         m_axi_rready  => rready_s);

   -- Load program, release reset
   read_file_proc : process
      variable row : line;
      variable i   : integer := 0;
   begin
      reset       <= '0';
      wea_instr_s <= (others => '1');
      wait until rising_edge(clk);
      while (not endfile(RISCV_instructions)) loop
         readline(RISCV_instructions, row);
         if (row'length > 0) then
            addra_instr_s <= std_logic_vector(to_unsigned(i, WADDR));
            dina_instr_s  <= to_std_logic_vector(string(row));
         end if;
         i             := i + 4;
         wait until rising_edge(clk);
      end loop;
      wea_instr_s <= (others => '0');
      reset       <= '1' after 20 ns;
      wait;
   end process;

   -- Clock generator
   clk_proc : process
   begin
      clk <= '1', '0' after 50 ns;
      wait for 100 ns;
   end process;

   -- Counters: 0 cycles, 1 commits, 2 rob_empty, 3 refill, 4 mispredicts, 5 jalr,
   -- 6 rob_full, 7 freelist, 8 lsq_full, 9 iq_full, 10 ex_busy
   cycle_count: process(clk)
      alias commit_valid is <<signal .riscv_top_tb.TOP_RISCV_1.data_path_1.rob_commit_valid_s : std_logic>>;
      alias rob_empty    is <<signal .riscv_top_tb.TOP_RISCV_1.data_path_1.rob_empty_s : std_logic>>;
      alias ex_valid     is <<signal .riscv_top_tb.TOP_RISCV_1.data_path_1.ex_valid_s : std_logic>>;
      alias ex_type      is <<signal .riscv_top_tb.TOP_RISCV_1.data_path_1.execute_instr_type_s : std_logic_vector(3 downto 0)>>;
      alias flush        is <<signal .riscv_top_tb.TOP_RISCV_1.flush_pipe_s : std_logic>>;
      alias stall_rn     is <<signal .riscv_top_tb.TOP_RISCV_1.stall_rn_s : std_logic>>;
      alias rob_full     is <<signal .riscv_top_tb.TOP_RISCV_1.rob_full_s : std_logic>>;
      alias fl_empty     is <<signal .riscv_top_tb.TOP_RISCV_1.free_list_fifo_empty_s : std_logic>>;
      alias lsq_full     is <<signal .riscv_top_tb.TOP_RISCV_1.lsq_full_s : std_logic>>;
      alias iq_full      is <<signal .riscv_top_tb.TOP_RISCV_1.iq_full_s : std_logic>>;
      alias ex_busy      is <<signal .riscv_top_tb.TOP_RISCV_1.ex_busy_s : std_logic>>;
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
      variable cnt : integer := 0;
   begin
      if rising_edge(clk) then
         if doutb_instr_s = std_logic_vector(to_unsigned(0, 32)) then
               cnt := cnt + 1;
               if cnt > 200 then
                  std.env.stop;
               end if;
         else
               cnt := 0;
         end if;
      end if;
   end process;

end architecture;

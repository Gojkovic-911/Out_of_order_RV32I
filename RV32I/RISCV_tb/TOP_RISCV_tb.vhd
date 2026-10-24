library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;
use work.txt_util.all;

library std;
use std.env.all;                     -- for std.env.stop

entity TOP_RISCV_tb is
generic (
        Vivado_version    : std_logic := '0';
        WADDR             : natural   := 14;    -- 16 KB instruction and data memory
        DATA_MEM_MAX_WAIT : natural   := 3;     -- AXI data memory: 0..N wait cycles per handshake
        -- Program to run; default is relative to the project simulation directory,
        -- the non-project flow (make np) passes an absolute path
        PROGRAM_FILE      : string    := "../../../../../../RISCV_tb/assembly_code_bin.txt"
    );
-- port ();
end entity;

architecture Behavioral of TOP_RISCV_tb is
   -- File containing the binary instructions (assembly code)
   file RISCV_instructions             : text open read_mode is PROGRAM_FILE;
   -- Global signals
   signal clk                          : std_logic := '0';
   signal reset                        : std_logic;
   -- Instruction memory signals
   signal ena_instr_s, enb_instr_s     : std_logic;
   signal wea_instr_s, web_instr_s     : std_logic_vector(3 downto 0);
   signal addra_instr_s, addrb_instr_s : std_logic_vector(WADDR-1 downto 0);
   signal dina_instr_s, dinb_instr_s   : std_logic_vector(31 downto 0);
   signal douta_instr_s, doutb_instr_s : std_logic_vector(31 downto 0);
   signal addrb_instr_32_s             : std_logic_vector(31 downto 0);
   -- Core data port
   signal dmem_req_s, dmem_ready_s     : std_logic;
   signal dmem_we_s, dmem_rsp_s        : std_logic;
   signal dmem_be_s                    : std_logic_vector(3 downto 0);
   signal dmem_addr_s                  : std_logic_vector(31 downto 0);
   signal dmem_wdata_s, dmem_rdata_s   : std_logic_vector(31 downto 0);
   -- AXI4-Lite between the bridge and the data memory
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

   -- Instruction memory
   -- Port A: used to initialize the instruction memory
   -- Port B: used by the processor to fetch instructions
   -- Constants:
   ena_instr_s   <= '1';
   enb_instr_s   <= '1';
   addrb_instr_s <= addrb_instr_32_s(WADDR-1 downto 0);
   web_instr_s   <= (others => '0');
   dinb_instr_s  <= (others => '0');
   -- Instance:
   instruction_mem : entity work.BRAM(behavioral)
      generic map(WADDR => WADDR)
      port map (clk      => clk,
                -- port A
                en_a_i   => ena_instr_s,
                we_a_i   => wea_instr_s,
                addr_a_i => addra_instr_s,
                data_a_i => dina_instr_s,
                data_a_o => douta_instr_s,
                -- port B
                en_b_i   => enb_instr_s,
                we_b_i   => web_instr_s,
                addr_b_i => addrb_instr_s,
                data_b_i => dinb_instr_s,
                data_b_o => doutb_instr_s);


   -- Data memory: core data port -> AXI4-Lite bridge -> AXI-Lite memory
   -- with random wait states on every channel
   axi_bridge : entity work.axi_lite_master
      port map (clk           => clk,
                reset         => reset,
                req_valid_i   => dmem_req_s,
                req_ready_o   => dmem_ready_s,
                req_we_i      => dmem_we_s,
                req_addr_i    => dmem_addr_s,
                req_wdata_i   => dmem_wdata_s,
                req_be_i      => dmem_be_s,
                rsp_valid_o   => dmem_rsp_s,
                rsp_rdata_o   => dmem_rdata_s,
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


   -- Top module - RISC-V processor core
   TOP_RISCV_1 : entity work.TOP_RISCV
      port map (
         clk   => clk,
         reset => reset,

         instr_mem_data_i   => doutb_instr_s,
         instr_mem_addr_o   => addrb_instr_32_s,

         data_mem_req_o     => dmem_req_s,
         data_mem_ready_i   => dmem_ready_s,
         data_mem_we_o      => dmem_we_s,
         data_mem_rsp_i     => dmem_rsp_s,
         data_mem_be_o      => dmem_be_s,
         data_mem_addr_o    => dmem_addr_s,
         data_mem_rdata_i   => dmem_rdata_s,
         data_mem_wdata_o   => dmem_wdata_s);

   -- Instruction memory initialization
   -- The program to be executed by the processor is loaded into memory
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

   -- Process that monitors doutb_instr_s
   -- If it is 0 for 200 consecutive cycles, terminate the simulation (clean stop)
   process(clk)
      variable cnt : integer := 0;
   begin
      if rising_edge(clk) then
         if doutb_instr_s = std_logic_vector(to_unsigned(0, 32)) then
               cnt := cnt + 1;
               if cnt > 200 then
                  -- report "doutb_instr_s has been 0 for 200 cycles – stopping simulation";
                  std.env.stop;   -- clean stop (VHDL-2008)
               end if;
         else
               cnt := 0;
         end if;
      end if;
   end process;

end architecture;

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
        WADDR             : natural   := 11
    );
-- port ();
end entity;

architecture Behavioral of TOP_RISCV_tb is
   -- File containing the binary instructions (assembly code)
   file RISCV_instructions             : text open read_mode is "../../../../../../RISCV_tb/assembly_code_bin.txt";
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
   -- Data memory signals
   signal ena_data_s, enb_data_s       : std_logic;
   signal wea_data_s, web_data_s       : std_logic_vector(3 downto 0);
   signal addra_data_s, addrb_data_s   : std_logic_vector(WADDR-1 downto 0);
   signal dina_data_s, dinb_data_s     : std_logic_vector(31 downto 0);
   signal douta_data_s, doutb_data_s   : std_logic_vector(31 downto 0);
   signal addra_data_32_s              : std_logic_vector(31 downto 0);

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


   -- Data memory
   -- Port A: used by the processor to read/write data
   -- Port B: unused
   -- Constants:
   addra_data_s <= addra_data_32_s(WADDR-1 downto 0);
   addrb_data_s <= (others => '0');
   dinb_data_s  <= (others => '0');
   ena_data_s   <= '1';
   enb_data_s   <= '1';
   -- Instance:
   data_mem : entity work.BRAM(behavioral)
      generic map(WADDR => WADDR)
      port map (clk      => clk,
                -- port A
                en_a_i   => ena_data_s,
                we_a_i   => wea_data_s,
                addr_a_i => addra_data_s,
                data_a_i => dina_data_s,
                data_a_o => douta_data_s,
                -- port B
                en_b_i   => enb_data_s,
                we_b_i   => web_data_s,
                addr_b_i => addrb_data_s,
                data_b_i => dinb_data_s,
                data_b_o => doutb_data_s);


   -- Top module - RISC-V processor core
   TOP_RISCV_1 : entity work.TOP_RISCV
      port map (
         clk   => clk,
         reset => reset,

         instr_mem_data_i   => doutb_instr_s,
         instr_mem_addr_o   => addrb_instr_32_s,

         data_mem_be_o      => wea_data_s,
         data_mem_addr_o    => addra_data_32_s,
         data_mem_rdata_i   => douta_data_s,
         data_mem_wdata_o   => dina_data_s);

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
   -- If it is 0 for 25 consecutive cycles, terminate the simulation (clean stop)
   process(clk)
      variable cnt : integer := 0;
   begin
      if rising_edge(clk) then
         if doutb_instr_s = std_logic_vector(to_unsigned(0, 32)) then
               cnt := cnt + 1;
               if cnt > 50 then
                  -- report "doutb_instr_s has been 0 for 25 cycles – stopping simulation";
                  std.env.stop;   -- clean stop (VHDL-2008)
               end if;
         else
               cnt := 0;
         end if;
      end if;
   end process;

end architecture;

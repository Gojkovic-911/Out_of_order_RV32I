library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity riscv_top_mem is
   generic 
           (DATA_WIDTH  : positive := 32;
            MEM_BYTES   : positive := 4;
            WADDR       : natural := 10);
   port(
      -- ********* Globalna sinhronizacija ******************
      clk                 : in  std_logic;
      reset               : in  std_logic
      
      );
end entity;

architecture rtl of riscv_top_mem is

   signal instr_addr    : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal instr_rdata   : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal data_addr     : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal data_we       : std_logic_vector(MEM_BYTES-1   downto 0);
   signal data_wdata    : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal data_rdata    : std_logic_vector(DATA_WIDTH-1  downto 0);

begin

   CPU: entity work.TOP_RISCV
      generic map (
         DATA_WIDTH => 32,
         MEM_BYTES  => 4
      )
      port map (
         clk               => clk,
         reset             => reset,
         instr_mem_addr_o  => instr_addr,
         instr_mem_data_i  => instr_rdata,
         data_mem_be_o     => data_we,
         data_mem_addr_o   => data_addr,
         data_mem_wdata_o  => data_wdata,
         data_mem_rdata_i  => data_rdata
      );

   INSTR_MEM: entity work.BRAM
      generic map (
         WADDR => WADDR
      )
      port map (
         clk        => clk,
         en_a_i     => '1',
         en_b_i     => '0',
         data_a_i   => (others => '0'),
         data_b_i   => (others => '0'),
         addr_a_i   => instr_addr(WADDR-1 downto 0),
         addr_b_i   => (others => '0'),
         we_a_i     => "0000",
         we_b_i     => "0000",
         data_a_o   => instr_rdata,
         data_b_o   => open
      );

   DATA_MEM: entity work.BRAM
      generic map (
         WADDR => WADDR
      )
      port map (
         clk        => clk,
         en_a_i     => '1',
         en_b_i     => '0',
         data_a_i   => data_wdata,
         data_b_i   => (others => '0'),
         addr_a_i   => data_addr(WADDR-1 downto 0),
         addr_b_i   => (others => '0'),
         we_a_i     => data_we,
         we_b_i     => "0000",
         data_a_o   => data_rdata,
         data_b_o   => open
      );

end rtl;


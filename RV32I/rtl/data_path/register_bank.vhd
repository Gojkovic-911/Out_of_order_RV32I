library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--***************MODULE DESCRIPTION**************
--32-entry register bank
--Two read ports, one write port
--WIDTH sets register data width
--***********************************************
entity register_bank is
   generic (WIDTH : positive := 32);
   port (clk           : in  std_logic;
         reset         : in  std_logic;
         -- Read port 1
         rs1_address_i : in  std_logic_vector(4 downto 0);
         rs1_data_o    : out std_logic_vector(WIDTH - 1 downto 0);
         -- Read port 2
         rs2_address_i : in  std_logic_vector(4 downto 0);
         rs2_data_o    : out std_logic_vector(WIDTH - 1 downto 0);
         -- Write port
         rd_we_i       : in  std_logic;  -- write enable
         rd_address_i  : in  std_logic_vector(4 downto 0);
         rd_data_i     : in  std_logic_vector(WIDTH - 1 downto 0));

end entity;

architecture Behavioral of register_bank is
   type reg_bank is array (0 to 31) of std_logic_vector(31 downto 0);
   signal reg_bank_s : reg_bank;

begin
   -- Synchronous register write
   reg_bank_write : process (clk) is
   begin
      if (falling_edge(clk))then
         if (reset = '0')then
            reg_bank_s <= (others => (others => '0'));
         elsif (rd_we_i = '1') then
            reg_bank_s(to_integer(unsigned(rd_address_i))) <= rd_data_i;
         end if;
      end if;
   end process;
   
   -- Asynchronous read, x0 always 0
   reg_bank_read : process (rs1_address_i, rs2_address_i, reg_bank_s) is
   begin

      if(to_integer(unsigned(rs1_address_i)) = 0) then
         rs1_data_o <= std_logic_vector(to_unsigned(0, WIDTH));
      else
         rs1_data_o <= reg_bank_s(to_integer(unsigned(rs1_address_i)));
      end if;

      if(to_integer(unsigned(rs2_address_i)) = 0) then
         rs2_data_o <= std_logic_vector(to_unsigned(0, WIDTH));
      else
         rs2_data_o <= reg_bank_s(to_integer(unsigned(rs2_address_i)));
      end if;

   end process;

end architecture;

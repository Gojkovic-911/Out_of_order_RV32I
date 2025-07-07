library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--***************OPIS MODULA*********************
--Registarska banka sa dva interfejsa za citanje
--podataka i jednim interfejsom za upis podataka.
--Broj registara u banci je 32.
--WIDTH je parametar koji odredjuje sirinu poda-
--data u registrima
--***********************************************
entity register_bank is
   generic (WIDTH : positive := 32);
   port (clk           : in  std_logic;
         reset         : in  std_logic;
         -- Interfejs 1 za citanje podataka
         rs1_address_i : in  std_logic_vector(4 downto 0);
         rs1_data_o    : out std_logic_vector(WIDTH - 1 downto 0);
         -- Interfejs 2 za citanje podataka
         rs2_address_i : in  std_logic_vector(4 downto 0);
         rs2_data_o    : out std_logic_vector(WIDTH - 1 downto 0);
         -- Interfejs za upis podataka
         rd_we_i       : in  std_logic;  -- port za dozvolu upisa
         rd_address_i  : in  std_logic_vector(4 downto 0);
         rd_data_i     : in  std_logic_vector(WIDTH - 1 downto 0));

end entity;

architecture Behavioral of register_bank is
   type reg_bank is array (0 to 31) of std_logic_vector(31 downto 0);
   signal reg_bank_s : reg_bank;

begin
   -- Sinhroni upis podataka u registartsku banku
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
   -- Asinhrono citanje podataka iz registartske banke. Po specifikaciji nulti
   -- registar je uvek 0.
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

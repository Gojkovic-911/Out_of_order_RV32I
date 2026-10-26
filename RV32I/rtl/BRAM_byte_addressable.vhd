library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity BRAM is
   generic
      (
         WADDR     : natural := 10;
         -- Initial contents from file
         INIT_FILE : string  := ""
         );
   port
      (
         clk      : in  std_logic;
         en_a_i   : in  std_logic;
         en_b_i   : in  std_logic;
         data_a_i : in  std_logic_vector(31 downto 0);
         data_b_i : in  std_logic_vector(31 downto 0);
         addr_a_i : in  std_logic_vector(WADDR - 1 downto 0);
         addr_b_i : in  std_logic_vector(WADDR - 1 downto 0);
         we_a_i   : in  std_logic_vector(3 downto 0);
         we_b_i   : in  std_logic_vector(3 downto 0);
         data_a_o : out std_logic_vector(31 downto 0);
         data_b_o : out std_logic_vector(31 downto 0)
         );

end BRAM;

architecture behavioral of BRAM is
   -- Word organized, byte enables
   constant WORDS : natural := 2**(WADDR - 2);
   type ram_type is array(0 to WORDS - 1) of std_logic_vector(31 downto 0);

   impure function read_init_file(name : string) return ram_type is
      file     f    : text open read_mode is name;
      variable row  : line;
      variable word : bit_vector(31 downto 0);
      variable ram  : ram_type := (others => (others => '0'));
      variable a    : natural  := 0;
   begin
      while (not endfile(f)) and (a < WORDS) loop
         readline(f, row);
         if row'length > 0 then
            read(row, word);
            ram(a) := to_stdlogicvector(word);
            a := a + 1;
         end if;
      end loop;
      return ram;
   end function;

   impure function init_ram return ram_type is
   begin
      if INIT_FILE = "" then
         return (others => (others => '0'));
      end if;
      return read_init_file(INIT_FILE);
   end function;

   signal ram_s : ram_type := init_ram;

begin

   -- Read first, byte writes
   process(clk)
      variable a_idx : natural range 0 to WORDS - 1;
      variable b_idx : natural range 0 to WORDS - 1;
   begin
      if(rising_edge(clk)) then
         a_idx := to_integer(unsigned(addr_a_i(WADDR - 1 downto 2)));
         b_idx := to_integer(unsigned(addr_b_i(WADDR - 1 downto 2)));
         if(en_a_i = '1') then
            data_a_o <= ram_s(a_idx);
            for i in 0 to 3 loop
               if(we_a_i(i) = '1') then
                  ram_s(a_idx)(8*i + 7 downto 8*i) <= data_a_i(8*i + 7 downto 8*i);
               end if;
            end loop;
         end if;
         if(en_b_i = '1') then
            data_b_o <= ram_s(b_idx);
         end if;
      end if;
   end process;

end behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity immediate is
   port (instr_i        : in  std_logic_vector (31 downto 0);
         instr_format_i : in  std_logic_vector ( 2 downto 0);
         imm_extended_o : out std_logic_vector (31 downto 0)
         );
end entity;

architecture Behavioral of immediate is
   signal extension_s        : std_logic_vector(26 downto 0);

begin

   extension_s <= (others => instr_i(31));

   -- na osnovu instrukcije iz prethodnog procesa, izdvoji i prosiri konstantu(immediate polje) na 32 bita
   process (instr_i, instr_format_i, extension_s) is
   begin
      case instr_format_i is
         when i_format_instruction =>
            imm_extended_o <= extension_s(19 downto 0) & instr_i(31 downto 20);
         when shamt_instruction  =>
            imm_extended_o <= extension_s & instr_i(24 downto 20);
         when b_format_instruction =>
            imm_extended_o <= extension_s(18 downto 0) & instr_i(31) & instr_i(7) & instr_i(30 downto 25) & instr_i(11 downto 8) & '0';
         when s_format_instruction =>
            imm_extended_o <= extension_s(19 downto 0) & instr_i(31 downto 25) & instr_i(11 downto 7);
         when j_format_instruction =>
            imm_extended_o <= extension_s(10 downto 0) & instr_i(31) & instr_i(19 downto 12) & instr_i(20) & instr_i(30 downto 21) & '0';
         when u_format_instruction =>
            imm_extended_o <= instr_i(31 downto 12) & (11 downto 0 => '0');
         when others =>
            imm_extended_o <= (others => '0');
      end case;
   end process;
end architecture;

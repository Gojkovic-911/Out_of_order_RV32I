library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity immediate is
   port (instruction_i        : in  std_logic_vector (31 downto 0);
         immediate_extended_o : out std_logic_vector (31 downto 0);
         instruction_type_o   : out std_logic_vector (2 downto 0));
end entity;

architecture Behavioral of immediate is
   signal opcode_s           : std_logic_vector(6 downto 0);
   signal instruction_type_s : std_logic_vector(2 downto 0);
   signal funct3_s           : std_logic_vector(2 downto 0);
   signal extension_s        : std_logic_vector(26 downto 0);

begin

   instruction_type_o <= instruction_type_s;
   opcode_s    <= instruction_i(6 downto 0);
   extension_s <= (others => instruction_i(31));
   funct3_s    <= instruction_i(14 downto 12);

   -- u odnosu na opcode_s pronadji instrukciju
   process (opcode_s) is
   begin
      case opcode_s(6 downto 2) is
         when "01100" =>
            instruction_type_s <= r_type_instruction;
         when "00100" =>
            if( funct3_s = "101" or funct3_s = "001")  then
                instruction_type_s <= shamt_instruction;
            else
                instruction_type_s <= i_type_instruction;
            end if;
         when "01000" =>
            instruction_type_s <= s_type_instruction;
         when "11000" =>
            instruction_type_s <= b_type_instruction;
         when "11001" | "00000"=>
            instruction_type_s <= i_type_instruction;
         when "11011" =>
            instruction_type_s <= j_type_instruction;
         when "01101" | "00101" =>
            instruction_type_s <= u_type_instruction;
         when others =>
            instruction_type_s <= r_type_instruction;
      end case;
   end process;

   -- na osnovu instrukcije iz prethodnog procesa, izdvoji i prosiri konstantu(immediate polje) na 32 bita
   process (instruction_i, instruction_type_s, extension_s) is
   begin
      case instruction_type_s is
         when i_type_instruction =>
            immediate_extended_o <= extension_s(19 downto 0) & instruction_i(31 downto 20);
         when shamt_instruction  =>
            immediate_extended_o <= extension_s & instruction_i(24 downto 20);
         when b_type_instruction =>
            immediate_extended_o <= extension_s(18 downto 0) & instruction_i(31) & instruction_i(7) & instruction_i(30 downto 25) & instruction_i(11 downto 8) & '0';
         when s_type_instruction =>
            immediate_extended_o <= extension_s(19 downto 0) & instruction_i(31 downto 25) & instruction_i(11 downto 7);
         when j_type_instruction =>
            immediate_extended_o <= extension_s(10 downto 0) & instruction_i(31) & instruction_i(19 downto 12) & instruction_i(20) & instruction_i(30 downto 21) & '0';
         when u_type_instruction =>
            immediate_extended_o <= instruction_i(31 downto 12) & (11 downto 0 => '0');
         when others =>
            immediate_extended_o <= (others => '0');
      end case;
   end process;
end architecture;

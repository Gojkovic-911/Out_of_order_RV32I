library ieee;
use ieee.std_logic_1164.all;

package instr_types_pkg is

   constant r_type_instruction : std_logic_vector(2 downto 0) := "000";
   constant i_type_instruction : std_logic_vector(2 downto 0) := "001";
   constant s_type_instruction : std_logic_vector(2 downto 0) := "010";
   constant b_type_instruction : std_logic_vector(2 downto 0) := "011";
   constant shamt_instruction  : std_logic_vector(2 downto 0) := "110"; 
   constant j_type_instruction : std_logic_vector(2 downto 0) := "101";
   constant u_type_instruction : std_logic_vector(2 downto 0) := "100";

   --constant fence_ecall_ebreak : std_logic_vector(2 downto 0):= "111";


end package alu_ops_pkg;

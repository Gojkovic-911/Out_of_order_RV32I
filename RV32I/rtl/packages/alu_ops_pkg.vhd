library ieee;
use ieee.std_logic_1164.all;

package alu_ops_pkg is

   -- ALU operation codes
   constant and_op    : std_logic_vector (4 downto 0) := "00000";
   constant or_op     : std_logic_vector (4 downto 0) := "00001";
   constant xor_op    : std_logic_vector (4 downto 0) := "00011";
   constant add_op    : std_logic_vector (4 downto 0) := "00010";
   constant sub_op    : std_logic_vector (4 downto 0) := "00110";
   constant eq_op     : std_logic_vector (4 downto 0) := "10111";
   constant slt_op    : std_logic_vector (4 downto 0) := "10100";
   constant sltu_op   : std_logic_vector (4 downto 0) := "10101";
   constant sll_op    : std_logic_vector (4 downto 0) := "10110";
   constant srl_op    : std_logic_vector (4 downto 0) := "00111";
   constant sra_op    : std_logic_vector (4 downto 0) := "01000";
   constant mulu_op   : std_logic_vector (4 downto 0) := "01001";
   constant mulhs_op  : std_logic_vector (4 downto 0) := "01010";
   constant mulhsu_op : std_logic_vector (4 downto 0) := "01011";
   constant mulhu_op  : std_logic_vector (4 downto 0) := "01100";
   constant divu_op   : std_logic_vector (4 downto 0) := "01101";
   constant divs_op   : std_logic_vector (4 downto 0) := "01110";
   constant remu_op   : std_logic_vector (4 downto 0) := "01111";
   constant rems_op   : std_logic_vector (4 downto 0) := "10000";

end package alu_ops_pkg;

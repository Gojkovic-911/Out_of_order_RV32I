library ieee;
use ieee.std_logic_1164.all;

package instr_types_pkg is
    
    -- Format of instructions (for immediate module)
    constant r_format_instruction : std_logic_vector(2 downto 0) := "000";
    constant i_format_instruction : std_logic_vector(2 downto 0) := "001";
    constant s_format_instruction : std_logic_vector(2 downto 0) := "010";
    constant b_format_instruction : std_logic_vector(2 downto 0) := "011";
    constant shamt_instruction    : std_logic_vector(2 downto 0) := "110"; 
    constant j_format_instruction : std_logic_vector(2 downto 0) := "101";
    constant u_format_instruction : std_logic_vector(2 downto 0) := "100";
    --constant fence_ecall_ebreak : std_logic_vector(2 downto 0):= "111";
   
    -- Instruction types
    constant R_TYPE : std_logic_vector(3 downto 0) := "0000";
    constant I_TYPE : std_logic_vector(3 downto 0) := "0001";
    constant LOAD   : std_logic_vector(3 downto 0) := "0010";
    constant STORE  : std_logic_vector(3 downto 0) := "0011";
    constant BRANCH : std_logic_vector(3 downto 0) := "0100";
    constant JAL    : std_logic_vector(3 downto 0) := "0101";
    constant JALR   : std_logic_vector(3 downto 0) := "0110";
    constant LUI    : std_logic_vector(3 downto 0) := "0111";
    constant AUIPC  : std_logic_vector(3 downto 0) := "1000";
    
    -- Instruction subtypes
    -- LOAD types
    constant NONE_L : std_logic_vector(4 downto 0) := "00111";
    constant LB     : std_logic_vector(4 downto 0) := "00000";
    constant LH     : std_logic_vector(4 downto 0) := "00001";
    constant LW     : std_logic_vector(4 downto 0) := "00010";
    constant LBU    : std_logic_vector(4 downto 0) := "00100";
    constant LHU    : std_logic_vector(4 downto 0) := "00101";
    
    -- STORE types
    constant NONE_S : std_logic_vector(4 downto 0) := "01111";
    constant SB     : std_logic_vector(4 downto 0) := "01000";
    constant SH     : std_logic_vector(4 downto 0) := "01001";
    constant SW     : std_logic_vector(4 downto 0) := "01010";
    
    -- BRANCH types
    constant NONE_B : std_logic_vector(4 downto 0) := "10000";
    constant BEQ    : std_logic_vector(4 downto 0) := "10001";
    constant BNE    : std_logic_vector(4 downto 0) := "10010";
    constant BLT    : std_logic_vector(4 downto 0) := "10011";
    constant BGE    : std_logic_vector(4 downto 0) := "10100";
    constant BLTU   : std_logic_vector(4 downto 0) := "10101";
    constant BGEU   : std_logic_vector(4 downto 0) := "10110";

    
    -- Instruction opcode constants 
    constant OPCODE_R_TYPE : std_logic_vector(6 downto 0) := "0110011";
    constant OPCODE_I_TYPE : std_logic_vector(6 downto 0) := "0010011";
    constant OPCODE_LOAD   : std_logic_vector(6 downto 0) := "0000011";
    constant OPCODE_STORE  : std_logic_vector(6 downto 0) := "0100011";
    constant OPCODE_BRANCH : std_logic_vector(6 downto 0) := "1100011";
    constant OPCODE_JAL    : std_logic_vector(6 downto 0) := "1101111";
    constant OPCODE_JALR   : std_logic_vector(6 downto 0) := "1100111";
    constant OPCODE_LUI    : std_logic_vector(6 downto 0) := "0110111";
    constant OPCODE_AUIPC  : std_logic_vector(6 downto 0) := "0010111";
    
end package instr_types_pkg;

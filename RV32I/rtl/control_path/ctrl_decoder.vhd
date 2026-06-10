library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity ctrl_dec is
    port (
        instr_i      : in  std_logic_vector(31 downto 0);
        
        rd_we_o         : out std_logic;   -- instruction has rd
        rs1_used_o      : out std_logic;   -- instruction 
        rs2_used_o      : out std_logic;   
        
        instr_format_o  : out std_logic_vector(2 downto 0);    -- For the immediate extension module
        instr_type_o    : out std_logic_vector(3 downto 0);
        
        -- ALU
        alu_2bit_op_o   : out std_logic_vector(1 downto 0);
        funct3_o        : out std_logic_vector(2 downto 0);
        funct7_o        : out std_logic_vector(6 downto 0)
    );
end entity;

architecture Behavioral of ctrl_dec is

    signal opcode  : std_logic_vector(6 downto 0);
begin
    
    opcode      <= instr_i(6 downto 0);
    funct3_o    <= instr_i(14 downto 12);
    funct7_o    <= instr_i(31 downto 25);

    process(opcode) is
    begin
        -- Default outputs
        rd_we_o      <= '0';
        rs1_used_o   <= '0';
        rs2_used_o   <= '0';
        
        instr_format_o  <= r_format_instruction; -- no immediate
        instr_type_o    <= (others => '0');
        
        alu_2bit_op_o   <= "00";
        
        -- Decode opcode
        if opcode = OPCODE_R_TYPE then
            -- R-type instructions
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            rs2_used_o      <= '1';
            
            alu_2bit_op_o   <= "01";
            
            instr_format_o  <= r_format_instruction;
            instr_type_o    <= R_TYPE;

        elsif opcode = OPCODE_I_TYPE then
            -- I-type ALU immediate
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            instr_format_o  <= i_format_instruction; -- I-type immediate
            alu_2bit_op_o   <= "10";
            instr_type_o    <= I_TYPE;

        elsif opcode = OPCODE_LOAD then
            -- Load instructions
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            instr_format_o  <= i_format_instruction; -- Load immediate
            alu_2bit_op_o   <= "00";
            instr_type_o    <= LOAD;

        elsif opcode = OPCODE_STORE then
            -- Store instructions
            rs1_used_o      <= '1';
            rs2_used_o      <= '1';
            instr_format_o  <= s_format_instruction; -- Store immediate
            alu_2bit_op_o   <= "00";
            instr_type_o    <= STORE;

        elsif opcode = OPCODE_BRANCH then
            -- Branch instructions
            rs1_used_o      <= '1';
            rs2_used_o      <= '1';
            instr_format_o  <= b_format_instruction; -- Branch immediate
            instr_type_o    <= BRANCH;

        elsif opcode = OPCODE_JAL then
            -- JAL
            rd_we_o         <= '1';
            instr_format_o  <= j_format_instruction; -- JAL immediate
            instr_type_o    <= JAL;

        elsif opcode = OPCODE_JALR then
            -- JALR
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            instr_format_o  <= i_format_instruction; -- JALR immediate
            alu_2bit_op_o   <= "00";
            instr_type_o    <= JALR;

        elsif opcode = OPCODE_LUI then
            -- LUI
            rd_we_o         <= '1';
            instr_format_o  <= u_format_instruction; -- LUI immediate
            alu_2bit_op_o   <= "00";
            instr_type_o    <= LUI;

        elsif opcode = OPCODE_AUIPC then
            -- AUIPC
            rd_we_o         <= '1';
            instr_format_o  <= u_format_instruction; -- AUIPC immediate
            alu_2bit_op_o   <= "00";
            instr_type_o    <= AUIPC;

        else
            -- Unknown opcode - disable everything
            null;
        end if;
    end process;

end Behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity ctrl_dec is
    port (
        instr_i      : in  std_logic_vector(31 downto 0);
        
        rd_we_o         : out std_logic;
        rs1_used_o      : out std_logic;
        rs2_used_o      : out std_logic;   
        
        instr_format_o  : out std_logic_vector(2 downto 0);
        instr_type_o    : out std_logic_vector(3 downto 0);
        
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
        
        instr_format_o  <= r_format_instruction;
        instr_type_o    <= (others => '0');
        
        alu_2bit_op_o   <= "00";
        
        -- Decode opcode
        if opcode = OPCODE_R_TYPE then
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            rs2_used_o      <= '1';
            
            alu_2bit_op_o   <= "01";
            
            instr_format_o  <= r_format_instruction;
            instr_type_o    <= R_TYPE;

        elsif opcode = OPCODE_I_TYPE then
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            instr_format_o  <= i_format_instruction;
            alu_2bit_op_o   <= "10";
            instr_type_o    <= I_TYPE;

        elsif opcode = OPCODE_LOAD then
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            instr_format_o  <= i_format_instruction;
            alu_2bit_op_o   <= "00";
            instr_type_o    <= LOAD;

        elsif opcode = OPCODE_STORE then
            rs1_used_o      <= '1';
            rs2_used_o      <= '1';
            instr_format_o  <= s_format_instruction;
            alu_2bit_op_o   <= "00";
            instr_type_o    <= STORE;

        elsif opcode = OPCODE_BRANCH then
            rs1_used_o      <= '1';
            rs2_used_o      <= '1';
            instr_format_o  <= b_format_instruction;
            instr_type_o    <= BRANCH;

        elsif opcode = OPCODE_JAL then
            rd_we_o         <= '1';
            instr_format_o  <= j_format_instruction;
            instr_type_o    <= JAL;

        elsif opcode = OPCODE_JALR then
            rd_we_o         <= '1';
            rs1_used_o      <= '1';
            instr_format_o  <= i_format_instruction;
            alu_2bit_op_o   <= "00";
            instr_type_o    <= JALR;

        elsif opcode = OPCODE_LUI then
            rd_we_o         <= '1';
            instr_format_o  <= u_format_instruction;
            alu_2bit_op_o   <= "00";
            instr_type_o    <= LUI;

        elsif opcode = OPCODE_AUIPC then
            rd_we_o         <= '1';
            instr_format_o  <= u_format_instruction;
            alu_2bit_op_o   <= "00";
            instr_type_o    <= AUIPC;

        else
            -- Unknown opcode
            null;
        end if;
    end process;

end Behavioral;

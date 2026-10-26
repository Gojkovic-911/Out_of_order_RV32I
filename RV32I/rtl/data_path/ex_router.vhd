library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

-- IS/EX steering to units
entity ex_router is
    generic (
        DATA_WIDTH      : natural := 32;
        ROB_ADDR_BITS   : natural := 6
    );
    port (
        valid_i             : in  std_logic;
        instr_type_i        : in  std_logic_vector(3 downto 0);
        rs1_data_i          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rs2_data_i          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        imm_i               : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        pc_i                : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rob_idx_i           : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        alu_full_i          : in  std_logic;
        branch_full_i       : in  std_logic;
        rs_full_o           : out std_logic;

        alu_valid_o         : out std_logic;
        alu_op_a_o          : out std_logic_vector(DATA_WIDTH-1 downto 0);
        alu_op_b_o          : out std_logic_vector(DATA_WIDTH-1 downto 0);

        branch_valid_o      : out std_logic;

        lsq_fill_valid_o    : out std_logic;
        lsq_fill_addr_o     : out std_logic_vector(DATA_WIDTH-1 downto 0);
        store_done_valid_o  : out std_logic;
        store_done_idx_o    : out std_logic_vector(ROB_ADDR_BITS-1 downto 0)
    );
end entity;

architecture Behavioral of ex_router is
    signal to_alu_s     : std_logic;
    signal to_branch_s  : std_logic;
    signal to_lsq_s     : std_logic;
begin

    to_alu_s    <= '1' when (instr_type_i = R_TYPE or instr_type_i = I_TYPE or instr_type_i = LUI or
                             instr_type_i = AUIPC or instr_type_i = JAL or instr_type_i = JALR) else '0';
    to_branch_s <= '1' when instr_type_i = BRANCH else '0';
    to_lsq_s    <= '1' when (instr_type_i = LOAD or instr_type_i = STORE) else '0';

    alu_valid_o     <= valid_i and to_alu_s;
    branch_valid_o  <= valid_i and to_branch_s;
    rs_full_o       <= (to_alu_s and alu_full_i) or (to_branch_s and branch_full_i);

    -- ALU operand select
    operands_mux:
    process (instr_type_i, rs1_data_i, rs2_data_i, imm_i, pc_i)
    begin
        case instr_type_i is
            when R_TYPE =>
                alu_op_a_o  <= rs1_data_i;
                alu_op_b_o  <= rs2_data_i;
            when I_TYPE =>
                alu_op_a_o  <= rs1_data_i;
                alu_op_b_o  <= imm_i;
            when LUI =>
                alu_op_a_o  <= (others => '0');
                alu_op_b_o  <= imm_i;
            when AUIPC =>
                alu_op_a_o  <= pc_i;
                alu_op_b_o  <= imm_i;
            when JAL | JALR =>
                alu_op_a_o  <= pc_i;
                alu_op_b_o  <= std_logic_vector(to_unsigned(4, DATA_WIDTH));
            when others =>
                alu_op_a_o  <= (others => '0');
                alu_op_b_o  <= (others => '0');
        end case;
    end process;

    -- AGU to LSQ
    lsq_fill_addr_o     <= std_logic_vector(unsigned(rs1_data_i) + unsigned(imm_i));
    lsq_fill_valid_o    <= valid_i and to_lsq_s;
    store_done_valid_o  <= valid_i when instr_type_i = STORE else '0';
    store_done_idx_o    <= rob_idx_i;

end Behavioral;

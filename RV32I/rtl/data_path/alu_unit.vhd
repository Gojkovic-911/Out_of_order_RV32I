library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ALU on IS/EX operands, result to CDB
entity alu_unit is
    generic (
        DATA_WIDTH      : natural := 32;
        PHYS_ADDR_BITS  : natural := 6;
        ROB_ADDR_BITS   : natural := 6
    );
    port (
        valid_i         : in  std_logic;
        op_a_i          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        op_b_i          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        subtype_i       : in  std_logic_vector(4 downto 0);
        rd_addr_i       : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rob_idx_i       : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- Result request to CDB
        cdb_req_o       : out std_logic;
        cdb_grant_i     : in  std_logic;
        cdb_data_o      : out std_logic_vector(DATA_WIDTH-1 downto 0);
        cdb_rd_addr_o   : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        cdb_rob_idx_o   : out std_logic_vector(ROB_ADDR_BITS-1 downto 0)
    );
end entity;

architecture Behavioral of alu_unit is
    signal alu_zero_s     : std_logic;
    signal alu_overflow_s : std_logic;
begin

    u_alu: entity work.ALU
        generic map (
            WIDTH => DATA_WIDTH
        )
        port map (
            a_i     => op_a_i,
            b_i     => op_b_i,
            op_i    => subtype_i,
            res_o   => cdb_data_o,
            zero_o  => alu_zero_s,
            of_o    => alu_overflow_s
        );

    cdb_req_o     <= valid_i;
    cdb_rd_addr_o <= rd_addr_i;
    cdb_rob_idx_o <= rob_idx_i;

end Behavioral;

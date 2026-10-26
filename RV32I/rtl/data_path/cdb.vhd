library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- CDB arbiter and register
entity cdb is
    generic (
        DATA_WIDTH      : natural := 32;
        PHYS_ADDR_BITS  : natural := 6;
        ROB_ADDR_BITS   : natural := 6
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;
        flush_i         : in  std_logic;

        alu_req_i       : in  std_logic;
        alu_grant_o     : out std_logic;
        alu_data_i      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        alu_rd_addr_i   : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        alu_rob_idx_i   : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        lsq_req_i       : in  std_logic;
        lsq_grant_o     : out std_logic;
        lsq_data_i      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        lsq_rd_addr_i   : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        lsq_rob_idx_i   : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- Broadcast to all consumers
        cdb_valid_o     : out std_logic;
        cdb_data_o      : out std_logic_vector(DATA_WIDTH-1 downto 0);
        cdb_addr_o      : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        cdb_rob_idx_o   : out std_logic_vector(ROB_ADDR_BITS-1 downto 0)
    );
end entity;

architecture Behavioral of cdb is
    signal last_grant_s : std_logic;
    signal grant_alu_s  : std_logic;
    signal grant_lsq_s  : std_logic;
begin

    -- Round-robin between ALU and LSQ
    arbiter: process(alu_req_i, lsq_req_i, last_grant_s)
    begin
        grant_alu_s <= '0';
        grant_lsq_s <= '0';
        case last_grant_s is
            when '0' =>
                if lsq_req_i = '1' then
                    grant_lsq_s <= '1';
                elsif alu_req_i = '1' then
                    grant_alu_s <= '1';
                end if;
            when others =>
                if alu_req_i = '1' then
                    grant_alu_s <= '1';
                elsif lsq_req_i = '1' then
                    grant_lsq_s <= '1';
                end if;
        end case;
    end process;

    alu_grant_o <= grant_alu_s;
    lsq_grant_o <= grant_lsq_s;

    -- CDB register, wrong path dropped
    cdb_reg: process(clk)
    begin
        if rising_edge(clk) then
            if (reset = '0') then
                cdb_data_o      <= (others => '0');
                cdb_addr_o      <= (others => '0');
                cdb_valid_o     <= '0';
                cdb_rob_idx_o   <= (others => '0');
                last_grant_s    <= '0';
            else
                if grant_alu_s = '1' then
                    cdb_data_o      <= alu_data_i;
                    cdb_addr_o      <= alu_rd_addr_i;
                    cdb_valid_o     <= not flush_i;
                    cdb_rob_idx_o   <= alu_rob_idx_i;
                    last_grant_s    <= '0';
                elsif grant_lsq_s = '1' then
                    cdb_data_o      <= lsq_data_i;
                    cdb_addr_o      <= lsq_rd_addr_i;
                    cdb_valid_o     <= not flush_i;
                    cdb_rob_idx_o   <= lsq_rob_idx_i;
                    last_grant_s    <= '1';
                else
                    cdb_data_o      <= (others => '0');
                    cdb_addr_o      <= (others => '0');
                    cdb_valid_o     <= '0';
                    cdb_rob_idx_o   <= (others => '0');
                end if;
            end if;
        end if;
    end process;

end Behavioral;

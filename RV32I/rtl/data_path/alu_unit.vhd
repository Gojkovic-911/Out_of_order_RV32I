library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ALU reservation station and ALU
entity alu_unit is
    generic (
        DATA_WIDTH      : natural := 32;
        PHYS_ADDR_BITS  : natural := 6;
        RS_DEPTH        : natural := 16;
        RS_BITS         : natural := 4;
        ROB_ADDR_BITS   : natural := 6
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;

        valid_i         : in  std_logic;
        rs1_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rs2_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rs1_ready_i     : in  std_logic;
        rs2_ready_i     : in  std_logic;
        op_a_i          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        op_b_i          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rd_addr_i       : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        subtype_i       : in  std_logic_vector(4 downto 0);
        rob_idx_i       : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        instruction_i   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        full_o          : out std_logic;
        flush_i         : in  std_logic;

        cdb_valid_i     : in  std_logic;
        cdb_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        cdb_data_i      : in  std_logic_vector(DATA_WIDTH-1 downto 0);

        -- Result request to CDB
        cdb_req_o       : out std_logic;
        cdb_grant_i     : in  std_logic;
        cdb_data_o      : out std_logic_vector(DATA_WIDTH-1 downto 0);
        cdb_rd_addr_o   : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        cdb_rob_idx_o   : out std_logic_vector(ROB_ADDR_BITS-1 downto 0)
    );
end entity;

architecture Behavioral of alu_unit is

    type rs_entry_t is record
        valid           : std_logic;
        rs1_ready       : std_logic;
        rs2_ready       : std_logic;
        rs1_data        : std_logic_vector(DATA_WIDTH-1 downto 0);
        rs2_data        : std_logic_vector(DATA_WIDTH-1 downto 0);
        rs1_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rs2_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        instr_subtype   : std_logic_vector(4 downto 0);
        rd_addr         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rob_idx         : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        instruction     : std_logic_vector(DATA_WIDTH-1 downto 0);
    end record;

    type rs_array_t is array (0 to RS_DEPTH-1) of rs_entry_t;
    signal rs_s             : rs_array_t;

    signal free_found_s     : std_logic;
    signal free_addr_s      : std_logic_vector(RS_BITS-1 downto 0);
    signal ready_found_s    : std_logic;
    signal ready_addr_s     : std_logic_vector(RS_BITS-1 downto 0);

    signal alu_a_s          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_b_s          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_op_s         : std_logic_vector(4 downto 0);
    signal alu_result_s     : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_zero_s       : std_logic;
    signal alu_overflow_s   : std_logic;

begin

    -- Write, wakeup, issue, flush
    rs_proc: process(clk)
        variable w : integer;
    begin
        if rising_edge(clk) then
            if reset = '0' then
                rs_s <= (others => (valid => '0', rs1_ready => '0', rs2_ready => '0', others => (others => '0')));
            else
                if (valid_i = '1' and free_found_s = '1') then
                    w := to_integer(unsigned(free_addr_s));
                    rs_s(w).valid           <= '1';
                    rs_s(w).rs1_addr        <= rs1_addr_i;
                    rs_s(w).rs2_addr        <= rs2_addr_i;
                    rs_s(w).rd_addr         <= rd_addr_i;
                    rs_s(w).instr_subtype   <= subtype_i;
                    rs_s(w).rob_idx         <= rob_idx_i;
                    rs_s(w).instruction     <= instruction_i;

                    if (rs1_addr_i = cdb_addr_i and cdb_valid_i = '1' and rs1_ready_i = '0') then
                        rs_s(w).rs1_ready   <= '1';
                        rs_s(w).rs1_data    <= cdb_data_i;
                    else
                        rs_s(w).rs1_ready   <= rs1_ready_i;
                        rs_s(w).rs1_data    <= op_a_i;
                    end if;

                    if (rs2_addr_i = cdb_addr_i and cdb_valid_i = '1' and rs2_ready_i = '0') then
                        rs_s(w).rs2_ready   <= '1';
                        rs_s(w).rs2_data    <= cdb_data_i;
                    else
                        rs_s(w).rs2_ready   <= rs2_ready_i;
                        rs_s(w).rs2_data    <= op_b_i;
                    end if;
                end if;

                if (ready_found_s = '1' and cdb_grant_i = '1') then
                    rs_s(to_integer(unsigned(ready_addr_s))).valid     <= '0';
                    rs_s(to_integer(unsigned(ready_addr_s))).rs1_ready <= '0';
                    rs_s(to_integer(unsigned(ready_addr_s))).rs2_ready <= '0';
                end if;

                if (cdb_valid_i = '1') then
                    for i in 0 to RS_DEPTH-1 loop
                        if (rs_s(i).valid = '1') then
                            if (rs_s(i).rs1_ready = '0' and rs_s(i).rs1_addr = cdb_addr_i) then
                                rs_s(i).rs1_ready <= '1';
                                rs_s(i).rs1_data  <= cdb_data_i;
                            end if;
                            if (rs_s(i).rs2_ready = '0' and rs_s(i).rs2_addr = cdb_addr_i) then
                                rs_s(i).rs2_ready <= '1';
                                rs_s(i).rs2_data  <= cdb_data_i;
                            end if;
                        end if;
                    end loop;
                end if;

                if (flush_i = '1') then
                    for i in 0 to RS_DEPTH-1 loop
                        rs_s(i).valid <= '0';
                    end loop;
                end if;
            end if;
        end if;
    end process;

    -- Free and ready entry search
    search: process(rs_s)
        variable idx   : integer;
        variable found : std_logic;
    begin
        found := '0';
        idx := 0;
        for i in 0 to RS_DEPTH-1 loop
            if (rs_s(i).valid = '0' and found = '0') then
                idx := i;
                found := '1';
            end if;
        end loop;
        free_found_s <= found;
        free_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));

        found := '0';
        idx := 0;
        for i in 0 to RS_DEPTH-1 loop
            if (rs_s(i).valid = '1' and rs_s(i).rs1_ready = '1' and rs_s(i).rs2_ready = '1' and found = '0') then
                idx := i;
                found := '1';
            end if;
        end loop;
        ready_found_s <= found;
        ready_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));
    end process;

    full_o <= not free_found_s;

    -- Granted entry into ALU
    alu_in: process(cdb_grant_i, ready_found_s, ready_addr_s, rs_s)
    begin
        alu_a_s         <= (others => '0');
        alu_b_s         <= (others => '0');
        alu_op_s        <= (others => '0');
        cdb_rd_addr_o   <= (others => '0');
        cdb_rob_idx_o   <= (others => '0');
        if (ready_found_s = '1' and cdb_grant_i = '1') then
            alu_a_s         <= rs_s(to_integer(unsigned(ready_addr_s))).rs1_data;
            alu_b_s         <= rs_s(to_integer(unsigned(ready_addr_s))).rs2_data;
            alu_op_s        <= rs_s(to_integer(unsigned(ready_addr_s))).instr_subtype;
            cdb_rd_addr_o   <= rs_s(to_integer(unsigned(ready_addr_s))).rd_addr;
            cdb_rob_idx_o   <= rs_s(to_integer(unsigned(ready_addr_s))).rob_idx;
        end if;
    end process;

    u_alu: entity work.ALU
        generic map (
            WIDTH => DATA_WIDTH
        )
        port map (
            a_i     => alu_a_s,
            b_i     => alu_b_s,
            op_i    => alu_op_s,
            res_o   => alu_result_s,
            zero_o  => alu_zero_s,
            of_o    => alu_overflow_s
        );

    cdb_req_o  <= ready_found_s;
    cdb_data_o <= alu_result_s;

end Behavioral;

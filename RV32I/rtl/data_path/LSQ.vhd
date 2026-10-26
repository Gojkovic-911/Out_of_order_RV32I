library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

-- Load/store queue, in-order head
entity LSQ is
    generic (
        DATA_WIDTH      : natural := 32;
        MEM_BYTES       : natural := 4;
        PHYS_ADDR_BITS  : natural := 6;
        ROB_ADDR_BITS   : natural := 6;
        LSQ_DEPTH       : natural := 16;
        LSQ_BITS        : natural := 4
    );
    port (
        clk                 : in  std_logic;
        reset               : in  std_logic;

        -- Allocation and AGU fill
        alloc_valid_i       : in  std_logic;
        alloc_is_store_i    : in  std_logic;
        lsq_tail_idx_o      : out std_logic_vector(LSQ_BITS-1 downto 0);
        lsq_full_o          : out std_logic;

        fill_valid_i        : in  std_logic;
        fill_idx_i          : in  std_logic_vector(LSQ_BITS-1 downto 0);
        fill_addr_i         : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        fill_wdata_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        fill_subtype_i      : in  std_logic_vector(4 downto 0);
        fill_rd_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        fill_rob_idx_i      : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- Commit and CDB
        commit_store_i      : in  std_logic;

        flush_pipe_i        : in  std_logic;

        cdb_req_o           : out std_logic;
        cdb_grant_i         : in  std_logic;
        cdb_data_o          : out std_logic_vector(DATA_WIDTH-1 downto 0);
        cdb_rd_addr_o       : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        cdb_rob_idx_o       : out std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- Data memory port
        data_mem_req_o      : out std_logic;
        data_mem_ready_i    : in  std_logic;
        data_mem_we_o       : out std_logic;
        data_mem_addr_o     : out std_logic_vector(DATA_WIDTH-1 downto 0);
        data_mem_wdata_o    : out std_logic_vector(DATA_WIDTH-1 downto 0);
        data_mem_be_o       : out std_logic_vector(MEM_BYTES-1 downto 0);
        data_mem_rsp_i      : in  std_logic;
        data_mem_rdata_i    : in  std_logic_vector(DATA_WIDTH-1 downto 0)
    );
end entity;

architecture Behavioral of LSQ is

    type lsq_entry_t is record
        valid           : std_logic;
        ready           : std_logic;
        is_store        : std_logic;
        addr            : std_logic_vector(DATA_WIDTH-1 downto 0);
        wdata           : std_logic_vector(DATA_WIDTH-1 downto 0);
        instr_subtype   : std_logic_vector(4 downto 0);
        rd_addr         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rob_idx         : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    end record;

    type lsq_array_t is array (0 to LSQ_DEPTH-1) of lsq_entry_t;
    signal lsq_s : lsq_array_t;

    signal head, tail           : integer range 0 to LSQ_DEPTH-1 := 0;

    -- One transaction in flight
    signal busy_s               : std_logic;
    signal busy_store_s         : std_logic;
    signal discard_s            : std_logic;
    
    signal head_load_s          : std_logic;
    signal load_done_s          : std_logic;
    signal load_data_s          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal load_ext_s           : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal head_addr_s          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal head_wdata_s         : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal head_subtype_s       : std_logic_vector(4 downto 0);
    
    signal committed_cnt        : integer range 0 to LSQ_DEPTH;
    
    signal issue_load_s         : std_logic;
    signal issue_store_s        : std_logic;
    signal load_rsp_s           : std_logic;
    signal store_done_s         : std_logic;
    signal load_pop_s           : std_logic;

begin

    head_load_s     <= lsq_s(head).valid and lsq_s(head).ready and (not lsq_s(head).is_store);
    
    -- Head request to memory
    issue_load_s    <= head_load_s and (not busy_s) and (not load_done_s) and (not flush_pipe_i);
    issue_store_s   <= lsq_s(head).valid and lsq_s(head).is_store and (not busy_s) when committed_cnt > 0 else '0';
    
    data_mem_req_o  <= issue_load_s or issue_store_s;
    data_mem_we_o   <= issue_store_s;
    
    load_rsp_s      <= data_mem_rsp_i and busy_s and (not busy_store_s) and (not discard_s);
    store_done_s    <= data_mem_rsp_i and busy_s and busy_store_s;
    
    cdb_req_o       <= load_done_s and head_load_s and (not flush_pipe_i);
    load_pop_s      <= load_done_s and head_load_s and (not flush_pipe_i) and cdb_grant_i;

    -- LSU for head entry
    head_addr_s     <= lsq_s(head).addr;
    head_wdata_s    <= lsq_s(head).wdata;
    head_subtype_s  <= lsq_s(head).instr_subtype;
    
    u_lsu: entity work.LSU
        generic map (
            DATA_WIDTH => DATA_WIDTH,
            MEM_BYTES  => MEM_BYTES
        )
        port map (
            data_mem_addr_i   => head_addr_s,
            data_mem_wdata_i  => head_wdata_s,
            data_mem_rdata_o  => load_ext_s,

            lsu_subtype_i     => head_subtype_s,

            data_mem_addr_o   => data_mem_addr_o,
            data_mem_wdata_o  => data_mem_wdata_o,
            data_mem_rdata_i  => data_mem_rdata_i,
            data_mem_be_o     => data_mem_be_o
        );

    cdb_data_o      <= load_data_s;
    cdb_rd_addr_o   <= lsq_s(head).rd_addr;
    cdb_rob_idx_o   <= lsq_s(head).rob_idx;

    lsq_tail_idx_o  <= std_logic_vector(to_unsigned(tail, LSQ_BITS));
    lsq_full_o      <= lsq_s(tail).valid;

    process(clk)
        variable committed_v : integer range 0 to LSQ_DEPTH;
    begin
        if rising_edge(clk) then
            if reset = '0' then
                head    <= 0;
                tail    <= 0;
                lsq_s   <= (others => (valid => '0', ready => '0', is_store => '0', others => (others => '0')));
                busy_s          <= '0';
                busy_store_s    <= '0';
                discard_s       <= '0';
                load_done_s     <= '0';
                load_data_s     <= (others => '0');
                committed_cnt   <= 0;
            else
                -- Memory transaction
                if (data_mem_ready_i = '1' and (issue_load_s = '1' or issue_store_s = '1')) then
                    busy_s          <= '1';
                    busy_store_s    <= issue_store_s;
                elsif (data_mem_rsp_i = '1' and busy_s = '1') then
                    busy_s          <= '0';
                    discard_s       <= '0';
                end if;
                
                if (load_rsp_s = '1') then
                    load_done_s     <= '1';
                    load_data_s     <= load_ext_s;
                elsif (load_pop_s = '1') then
                    load_done_s     <= '0';
                end if;
                
                -- Committed store count
                committed_v := committed_cnt;
                if (commit_store_i = '1') then
                    committed_v := committed_v + 1;
                end if;
                if (store_done_s = '1') then
                    committed_v := committed_v - 1;
                end if;
                committed_cnt <= committed_v;
                
                -- Allocate in program order
                if (alloc_valid_i = '1' and lsq_s(tail).valid = '0' and flush_pipe_i = '0') then
                    lsq_s(tail).valid       <= '1';
                    lsq_s(tail).ready       <= '0';
                    lsq_s(tail).is_store    <= alloc_is_store_i;
                    tail                    <= (tail + 1) mod LSQ_DEPTH;
                end if;

                if (fill_valid_i = '1' and lsq_s(to_integer(unsigned(fill_idx_i))).valid = '1') then
                    lsq_s(to_integer(unsigned(fill_idx_i))).ready   <= '1';
                    lsq_s(to_integer(unsigned(fill_idx_i))).addr    <= fill_addr_i;
                    lsq_s(to_integer(unsigned(fill_idx_i))).wdata   <= fill_wdata_i;
                    lsq_s(to_integer(unsigned(fill_idx_i))).instr_subtype <= fill_subtype_i;
                    lsq_s(to_integer(unsigned(fill_idx_i))).rd_addr <= fill_rd_addr_i;
                    lsq_s(to_integer(unsigned(fill_idx_i))).rob_idx <= fill_rob_idx_i;
                end if;

                if (load_pop_s = '1' or store_done_s = '1') then
                    lsq_s(head).valid   <= '0';
                    lsq_s(head).ready   <= '0';
                    head                <= (head + 1) mod LSQ_DEPTH;
                end if;

                -- Flush keeps committed stores
                if (flush_pipe_i = '1') then
                    committed_v := committed_cnt;
                    if (commit_store_i = '1') then
                        committed_v := committed_v + 1;
                    end if;
                    for i in 0 to LSQ_DEPTH-1 loop
                        if ((i - head + LSQ_DEPTH) mod LSQ_DEPTH >= committed_v) then
                            lsq_s(i).valid <= '0';
                        end if;
                    end loop;
                    tail        <= (head + committed_v) mod LSQ_DEPTH;
                    load_done_s <= '0';
                    -- In-flight load response dropped
                    if (busy_s = '1' and busy_store_s = '0' and data_mem_rsp_i = '0') then
                        discard_s <= '1';
                    end if;
                end if;
            end if;
        end if;
    end process;

end Behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity issue_module is
    generic (
        IQ_DEPTH       : natural := 16;
        IQ_BITS        : natural := 4;
        PHYS_ADDR_BITS : natural := 5;
        DATA_WIDTH     : natural := 32;
        ROB_ADDR_BITS  : natural := 6;
        LSQ_BITS       : natural := 4;
        PAYLOAD_RAM_STYLE : string := "distributed"
    );
    port (
        clk          : in  std_logic;
        reset        : in  std_logic;
        
        dispatch_valid_i         : in  std_logic;
        dispatch_rs1_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        dispatch_rs2_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        dispatch_rd_addr_i       : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        
        dispatch_rs1_ready_i     : in  std_logic;
        dispatch_rs2_ready_i     : in  std_logic;
        
        dispatch_imm_i           : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        dispatch_pc_reg_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        
        dispatch_instr_type_i    : in  std_logic_vector(3 downto 0); 
        dispatch_instr_subtype_i : in  std_logic_vector(4 downto 0);
        dispatch_rob_idx_i       : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        dispatch_lsq_idx_i       : in  std_logic_vector(LSQ_BITS-1 downto 0);
        dispatch_instruction    : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        
        cdb_valid_i              : in  std_logic;
        cdb_addr_i               : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        stall_iq_is_i            : in  std_logic;
        stall_is_i               : in std_logic;
        flush_pipe_i             : in std_logic;
        
        issue_valid_o            : out std_logic;
        issue_rs1_addr_o         : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        issue_rs2_addr_o         : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        issue_rd_addr_o          : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        issue_rob_idx_o          : out std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        issue_lsq_idx_o          : out std_logic_vector(LSQ_BITS-1 downto 0);
        
        issue_instr_type_o       : out std_logic_vector(3 downto 0);
        issue_instr_subtype_o    : out std_logic_vector(4 downto 0);
        
        issue_imm_o              : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        issue_pc_reg_o           : out std_logic_vector(DATA_WIDTH-1 downto 0);
        issue_instruction        : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        iq_full_o                : out std_logic
    );  
end entity;

architecture Behavioral of issue_module is
    
    -- IQ payload RAM
    type iq_entry_t is record
        instruction     : std_logic_vector(DATA_WIDTH-1 downto 0);
        rd_addr         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        imm             : std_logic_vector(DATA_WIDTH-1 downto 0);
        pc_value        : std_logic_vector(DATA_WIDTH-1 downto 0);
        instr_type      : std_logic_vector(3 downto 0);
        instr_subtype   : std_logic_vector(4 downto 0);
        rob_idx         : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        lsq_idx         : std_logic_vector(LSQ_BITS-1 downto 0);
    end record;

    constant IQ_WORD_BITS : natural := 3*DATA_WIDTH + PHYS_ADDR_BITS + 4 + 5 + ROB_ADDR_BITS + LSQ_BITS;

    function to_word(e : iq_entry_t) return std_logic_vector is
    begin
        return e.instruction & e.rd_addr & e.imm & e.pc_value & e.instr_type &
               e.instr_subtype & e.rob_idx & e.lsq_idx;
    end function;

    function to_entry(w : std_logic_vector(IQ_WORD_BITS-1 downto 0)) return iq_entry_t is
        variable e : iq_entry_t;
        variable p : natural := IQ_WORD_BITS;
    begin
        e.instruction   := w(p-1 downto p-DATA_WIDTH);      p := p - DATA_WIDTH;
        e.rd_addr       := w(p-1 downto p-PHYS_ADDR_BITS);  p := p - PHYS_ADDR_BITS;
        e.imm           := w(p-1 downto p-DATA_WIDTH);      p := p - DATA_WIDTH;
        e.pc_value      := w(p-1 downto p-DATA_WIDTH);      p := p - DATA_WIDTH;
        e.instr_type    := w(p-1 downto p-4);               p := p - 4;
        e.instr_subtype := w(p-1 downto p-5);               p := p - 5;
        e.rob_idx       := w(p-1 downto p-ROB_ADDR_BITS);   p := p - ROB_ADDR_BITS;
        e.lsq_idx       := w(p-1 downto 0);
        return e;
    end function;

    -- Flat words, so it maps to LUTRAM
    type iq_ram_t is array(0 to IQ_DEPTH-1) of std_logic_vector(IQ_WORD_BITS-1 downto 0);
    signal iq_ram_s   : iq_ram_t;
    signal iq_read_s  : iq_entry_t;
    attribute ram_style : string;
    attribute ram_style of iq_ram_s : signal is PAYLOAD_RAM_STYLE;
    
    -- IQ status flip-flops
    type iq_ff_entry_t is record
        valid           : std_logic;
        rs1_ready       : std_logic;
        rs2_ready       : std_logic;
        rs1_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rs2_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    end record;
    
    type iq_ffs_t is array (0 to IQ_DEPTH-1) of iq_ff_entry_t;
    signal iq_ffs_s : iq_ffs_t;
    
    -- IQ addressing signals
    signal dispatch_ready_s         : std_logic;
    signal dispatch_addr_s          : std_logic_vector(IQ_BITS-1  downto 0);
    
    signal issue_valid_s       : std_logic;
    signal issue_addr_s        : std_logic_vector(IQ_BITS-1  downto 0);
    
    signal issue_addr_reg_s    : std_logic_vector(IQ_BITS-1  downto 0);
    
    signal dispatch_ptr_s : integer range 0 to IQ_DEPTH-1 := 0;
    signal issue_ptr_s    : integer range 0 to IQ_DEPTH-1 := 0;
    
begin

    -- Payload RAM, no reset
    payload_wr: process(clk)
    begin
        if (rising_edge(clk)) then
            if (dispatch_ready_s = '1' and dispatch_valid_i = '1') then
                iq_ram_s(to_integer(unsigned(dispatch_addr_s))) <= to_word((
                    rd_addr       => dispatch_rd_addr_i,
                    instr_type    => dispatch_instr_type_i,
                    instr_subtype => dispatch_instr_subtype_i,
                    imm           => dispatch_imm_i,
                    pc_value      => dispatch_pc_reg_i,
                    rob_idx       => dispatch_rob_idx_i,
                    lsq_idx       => dispatch_lsq_idx_i,
                    instruction   => dispatch_instruction));
            end if;
        end if;
    end process;

    iq_read_s <= to_entry(iq_ram_s(to_integer(unsigned(issue_addr_s))));

    -- Registered payload read
    payload_rd: process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                issue_rd_addr_o       <= (others => '0');
                issue_instr_type_o    <= (others => '0');
                issue_instr_subtype_o <= (others => '0');
                issue_imm_o           <= (others => '0');
                issue_pc_reg_o        <= (others => '0');
                issue_rob_idx_o       <= (others => '0');
                issue_lsq_idx_o       <= (others => '0');
                issue_instruction     <= (others => '0');
                issue_rs1_addr_o      <= (others => '0');
                issue_rs2_addr_o      <= (others => '0');
            elsif (stall_iq_is_i = '0' and issue_valid_s = '1') then
                issue_rd_addr_o       <= iq_read_s.rd_addr;
                issue_instr_type_o    <= iq_read_s.instr_type;
                issue_instr_subtype_o <= iq_read_s.instr_subtype;
                issue_imm_o           <= iq_read_s.imm;
                issue_pc_reg_o        <= iq_read_s.pc_value;
                issue_rob_idx_o       <= iq_read_s.rob_idx;
                issue_lsq_idx_o       <= iq_read_s.lsq_idx;
                issue_instruction     <= iq_read_s.instruction;
                issue_rs1_addr_o      <= iq_ffs_s(to_integer(unsigned(issue_addr_s))).rs1_addr;
                issue_rs2_addr_o      <= iq_ffs_s(to_integer(unsigned(issue_addr_s))).rs2_addr;
            end if;
        end if;
    end process;

    -- Status flip-flops
    process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                issue_valid_o <= '0';
                
                for i in 0 to IQ_DEPTH-1 loop
                    iq_ffs_s(i).valid       <= '0';
                    iq_ffs_s(i).rs1_ready   <= '0';
                    iq_ffs_s(i).rs2_ready   <= '0';
                    iq_ffs_s(i).rs1_addr    <= (others => '0');
                    iq_ffs_s(i).rs2_addr    <= (others => '0');
                end loop;
            else
                -- Dispatch writes status
                if (dispatch_ready_s = '1') then
                    if (dispatch_valid_i = '1') then
                        iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).valid    <= '1';
                        iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs1_addr <= dispatch_rs1_addr_i;
                        iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs2_addr <= dispatch_rs2_addr_i;
                    
                        if(dispatch_rs1_addr_i = cdb_addr_i and cdb_valid_i = '1') then
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs1_ready   <= '1';
                        else
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs1_ready   <= dispatch_rs1_ready_i;
                        end if;
                        
                        if(dispatch_rs2_addr_i = cdb_addr_i and cdb_valid_i = '1') then
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs2_ready   <= '1';
                        else
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs2_ready   <= dispatch_rs2_ready_i;
                        end if;
                    end if;
                end if;
                
                -- Issue clears valid
                if (stall_iq_is_i = '0') then
                    if (issue_valid_s = '1') then
                        issue_valid_o <= '1';
                        iq_ffs_s(to_integer(unsigned(issue_addr_s))).valid    <= '0';
                    else
                        issue_valid_o <= '0';
                    end if;  
                end if;  
                
                -- CDB wakeup of operands
                if (cdb_valid_i = '1') then
                    for i in 0 to IQ_DEPTH-1 loop
                        if (iq_ffs_s(i).valid = '1') then
                            if (iq_ffs_s(i).rs1_addr = cdb_addr_i) then
                                iq_ffs_s(i).rs1_ready <= '1';
                            end if;
                            if (iq_ffs_s(i).rs2_addr = cdb_addr_i) then
                                iq_ffs_s(i).rs2_ready <= '1';
                            end if;
                        end if;
                    end loop;
                end if;
                
                -- Flush clears the IQ
                if(flush_pipe_i = '1') then   
                    issue_valid_o <= '0';
                    for i in 0 to IQ_DEPTH-1 loop
                        iq_ffs_s(i).valid <= '0';
                    end loop;
                end if;
            end if;
        end if;
    end process;
    
    -- Read cycle alignment register
    addr_reg:process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                issue_addr_reg_s       <= (others => '0');
                issue_ptr_s            <= 0;
                dispatch_ptr_s         <= 0;
            else
                if (stall_iq_is_i = '0') then
                    issue_addr_reg_s       <= issue_addr_s;
                    if (issue_valid_s = '1') then
                        issue_ptr_s        <= to_integer(unsigned(issue_addr_s));
                    end if;
                end if;
                
                -- Round-robin dispatch pointer
                if (dispatch_ready_s = '1' and dispatch_valid_i = '1') then
                    dispatch_ptr_s         <= to_integer(unsigned(dispatch_addr_s));
                end if;
            end if;
        end if;
    end process;
    
    -- Dispatch: free slot search
    dipatch_enc:process(iq_ffs_s, issue_addr_reg_s, dispatch_ptr_s)
        variable i    : integer;
    begin
        
        dispatch_ready_s <= '0';
        dispatch_addr_s  <= std_logic_vector(to_unsigned(0, IQ_BITS));
        iq_full_o <= '1';
        
        for k in 1 to IQ_DEPTH loop
            i := (dispatch_ptr_s + k) mod IQ_DEPTH;
            
            if iq_ffs_s(i).valid = '0' and issue_addr_reg_s /= std_logic_vector(to_unsigned(i, IQ_BITS)) then
                dispatch_ready_s <= '1';
                dispatch_addr_s  <= std_logic_vector(to_unsigned(i, IQ_BITS));
                iq_full_o <= '0';
                exit;
            end if;
        end loop;
    end process;
    
    -- Issue only fully ready entries
    process(iq_ffs_s, issue_ptr_s)
        variable idx  : integer;
        variable found: std_logic;
        variable i    : integer;
    begin
        idx := 0;
        found := '0';
        
        for k in 1 to IQ_DEPTH loop
            i := (issue_ptr_s + k) mod IQ_DEPTH;
            if  iq_ffs_s(i).rs1_ready = '1' and iq_ffs_s(i).rs2_ready = '1' and iq_ffs_s(i).valid = '1' then
                idx     := i;
                found   := '1';
                exit;
            end if;
        end loop;
        
        issue_valid_s <= found;
        issue_addr_s  <= std_logic_vector(to_unsigned(idx, IQ_BITS));
    end process;
end Behavioral;

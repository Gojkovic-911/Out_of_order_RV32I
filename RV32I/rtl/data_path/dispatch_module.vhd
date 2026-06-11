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
        ROB_ADDR_BITS  : natural := 6
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
        dispatch_instruction     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        
        cdb_valid_i              : in  std_logic;
        cdb_addr_i               : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        stall_iq_is_i            : in  std_logic;
        stall_is_i               : in std_logic;
        flush_pipe_i             : in std_logic;
        spec_instr_i             : in std_logic;
        clear_spec_i             : in  std_logic;
        
        issue_valid_o            : out std_logic; -- if found
        issue_rs1_addr_o         : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);    -- rs1 addr
        issue_rs2_addr_o         : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);    -- rs2 addr
        issue_rd_addr_o          : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);    -- rd addr
        issue_rob_idx_o          : out std_logic_vector(ROB_ADDR_BITS-1 downto 0);    -- rd addr
        
        issue_rs1_ready_o        : out std_logic;
        issue_rs2_ready_o        : out std_logic;
        
        issue_instr_type_o       : out std_logic_vector(3 downto 0);                 -- needed for selecting ex units
        issue_instr_subtype_o    : out std_logic_vector(4 downto 0);                 -- needed for ex units
        spec_instr_o             : out std_logic;
        
        issue_imm_o              : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        issue_pc_reg_o           : out std_logic_vector(DATA_WIDTH-1 downto 0);
        issue_instruction        : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        iq_full_o                : out std_logic
    );  
end entity;

architecture Behavioral of issue_module is
    
    -- IQ entries RAM
    type iq_entry_t is record
        instruction     : std_logic_vector(DATA_WIDTH-1 downto 0);
        rd_addr         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        imm             : std_logic_vector(DATA_WIDTH-1 downto 0);
        pc_value        : std_logic_vector(DATA_WIDTH-1 downto 0);
        instr_type      : std_logic_vector(3 downto 0);
        instr_subtype   : std_logic_vector(4 downto 0);
        rob_idx         : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    end record;
    
    type iq_ram_t is array(0 to IQ_DEPTH-1) of iq_entry_t;
    signal iq_ram_s : iq_ram_t;
    
    -- IQ entries flip-flops
    type iq_ff_entry_t is record
        valid           : std_logic;
        rs1_ready       : std_logic;
        rs2_ready       : std_logic;
        rs1_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rs2_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        is_jalr         : std_logic;
        is_spec         : std_logic;
    end record;
    
    type iq_ffs_t is array (0 to IQ_DEPTH-1) of iq_ff_entry_t;
    signal iq_ffs_s : iq_ffs_t;
    
    -- Valid and addr signals to address the IQ RAM/ffs
    signal dispatch_ready_s         : std_logic;
    signal dispatch_addr_s          : std_logic_vector(IQ_BITS-1  downto 0);
    
    signal issue_full_valid_s       : std_logic;
    signal issue_full_addr_s        : std_logic_vector(IQ_BITS-1  downto 0);
    
    signal issue_partial_valid_s    : std_logic;
    signal issue_partial_addr_s     : std_logic_vector(IQ_BITS-1  downto 0);
    
    signal issue_partial_addr_reg_s : std_logic_vector(IQ_BITS-1  downto 0);
    signal issue_full_addr_reg_s    : std_logic_vector(IQ_BITS-1  downto 0);
    
    signal issue_partial_valid_reg_s: std_logic;
    signal issue_full_valid_reg_s   : std_logic;
    
    signal dispatch_ptr_s : integer range 0 to IQ_DEPTH-1 := 0;
    signal issue_ptr_s    : integer range 0 to IQ_DEPTH-1 := 0;
    signal partial_ptr_s  : integer range 0 to IQ_DEPTH-1 := 0;
    signal issue_valid_s  : std_logic;
    
    
begin

    -- issue_valid_o <= issue_valid_s;
    
    -- IQ RAM
    -- Synchronous read/write
    process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                iq_full_o <= '0';
                iq_ram_s <= (others => (others => (others => '0')));
                
                issue_rd_addr_o       <= (others => '0');
                issue_instr_type_o    <= (others => '0');
                issue_instr_subtype_o <= (others => '0');
                issue_imm_o           <= (others => '0');
                issue_pc_reg_o        <= (others => '0');
                issue_rob_idx_o       <= (others => '0');
                issue_instruction     <= (others => '0');
                
            else
                -- issue_rd_addr_o       <= (others => '0');
                -- issue_instr_type_o    <= (others => '0');
                -- issue_instr_subtype_o <= (others => '0');
                -- issue_imm_o           <= (others => '0');
                -- issue_pc_reg_o        <= (others => '0');
                -- issue_rob_idx_o       <= (others => '0');
                -- issue_instruction     <= (others => '0');
                
                -- Dispatch
                -- Write into the Instruction Queue RAM
                iq_full_o <= '1';
                if (dispatch_ready_s = '1') then
                    iq_full_o <= '0';
                    
                    if (dispatch_valid_i = '1') then
                        iq_ram_s(to_integer(unsigned(dispatch_addr_s))).rd_addr         <= dispatch_rd_addr_i;
                        iq_ram_s(to_integer(unsigned(dispatch_addr_s))).instr_type      <= dispatch_instr_type_i;
                        iq_ram_s(to_integer(unsigned(dispatch_addr_s))).instr_subtype   <= dispatch_instr_subtype_i;
                        iq_ram_s(to_integer(unsigned(dispatch_addr_s))).imm             <= dispatch_imm_i;
                        iq_ram_s(to_integer(unsigned(dispatch_addr_s))).pc_value        <= dispatch_pc_reg_i;
                        iq_ram_s(to_integer(unsigned(dispatch_addr_s))).rob_idx         <= dispatch_rob_idx_i;
                        iq_ram_s(to_integer(unsigned(dispatch_addr_s))).instruction     <= dispatch_instruction;
                    end if;
                end if;
                
                -- Issue
                -- Read from the Instruction Queue RAM
                if (stall_iq_is_i = '0') then
                    if (issue_full_valid_s = '1') then
                        issue_rd_addr_o         <= iq_ram_s(to_integer(unsigned(issue_full_addr_s))).rd_addr;
                        issue_instr_type_o      <= iq_ram_s(to_integer(unsigned(issue_full_addr_s))).instr_type;
                        issue_instr_subtype_o   <= iq_ram_s(to_integer(unsigned(issue_full_addr_s))).instr_subtype;
                        issue_imm_o             <= iq_ram_s(to_integer(unsigned(issue_full_addr_s))).imm;
                        issue_pc_reg_o          <= iq_ram_s(to_integer(unsigned(issue_full_addr_s))).pc_value;
                        issue_rob_idx_o         <= iq_ram_s(to_integer(unsigned(issue_full_addr_s))).rob_idx;
                        issue_instruction       <= iq_ram_s(to_integer(unsigned(issue_full_addr_s))).instruction;
                        
                    elsif (issue_partial_valid_s = '1') then      
                        issue_rd_addr_o         <= iq_ram_s(to_integer(unsigned(issue_partial_addr_s))).rd_addr;
                        issue_instr_type_o      <= iq_ram_s(to_integer(unsigned(issue_partial_addr_s))).instr_type;
                        issue_instr_subtype_o   <= iq_ram_s(to_integer(unsigned(issue_partial_addr_s))).instr_subtype;
                        issue_imm_o             <= iq_ram_s(to_integer(unsigned(issue_partial_addr_s))).imm;
                        issue_pc_reg_o          <= iq_ram_s(to_integer(unsigned(issue_partial_addr_s))).pc_value;
                        issue_rob_idx_o         <= iq_ram_s(to_integer(unsigned(issue_partial_addr_s))).rob_idx;
                        issue_instruction       <= iq_ram_s(to_integer(unsigned(issue_partial_addr_s))).instruction;
                    end if;
                end if;
                
            end if;
        end if;
    end process;    
    
    -- IQ FFs
    -- Synchronous write
    process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                issue_valid_s <= '0';
                
                for i in 0 to IQ_DEPTH-1 loop
                    iq_ffs_s(i).valid       <= '0';
                    iq_ffs_s(i).rs1_ready   <= '0';
                    iq_ffs_s(i).rs2_ready   <= '0';
                    iq_ffs_s(i).is_jalr     <= '0';
                    iq_ffs_s(i).is_spec     <= '0';
                    iq_ffs_s(i).rs1_addr    <= (others => '0');
                    iq_ffs_s(i).rs2_addr    <= (others => '0');
                end loop;
            else
                        -- issue_valid_o <= '0';
                -- Dispatch
                -- Write into the Instruction Queue FFs
                if (dispatch_ready_s = '1') then
                    if (dispatch_valid_i = '1') then
                        iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).valid    <= '1';
                        iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs1_addr <= dispatch_rs1_addr_i;
                        iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs2_addr <= dispatch_rs2_addr_i;
                        iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).is_spec  <= spec_instr_i;
                    
                        -- Set operand ready fields if tag is published, else read from RN/IS reg
                        if(dispatch_rs1_addr_i = cdb_addr_i and cdb_valid_i = '1') then
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs1_ready   <= '1';
                        else
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs1_ready   <= dispatch_rs1_ready_i;
                        end if;
                        
                        -- Set operand ready fields if tag is published, else read from RN/IS reg
                        if(dispatch_rs2_addr_i = cdb_addr_i and cdb_valid_i = '1') then
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs2_ready   <= '1';
                        else
                            iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).rs2_ready   <= dispatch_rs2_ready_i;
                        end if;
                    end if;
                end if;
                
                -- Jalr can pass only if rs1 is ready
                if(dispatch_instr_type_i = JALR) then
                    iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).is_jalr     <= '1';
                else
                    iq_ffs_s(to_integer(unsigned(dispatch_addr_s))).is_jalr     <= '0';
                end if;
                        
                -- Issue
                -- Reset the valid bits for the instruction that is issued
                if (stall_iq_is_i = '0') then
                    if (issue_full_valid_s = '1') then
                        issue_valid_o <= '1';
                        iq_ffs_s(to_integer(unsigned(issue_full_addr_s))).valid    <= '0';
                    elsif (issue_partial_valid_s = '1') then    
                        issue_valid_o <= '1';
                        iq_ffs_s(to_integer(unsigned(issue_partial_addr_s))).valid <= '0';
                    else
                        issue_valid_o <= '0';
                    end if;  
                end if;  
                
                -- Update all the operand ready fields
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
                
                -- Branch misspredicted
                if(flush_pipe_i = '1') then   
                    for i in 0 to IQ_DEPTH-1 loop
                        if (iq_ffs_s(i).is_spec = '1' ) then
                            iq_ffs_s(i).valid <= '0';
                        end if;
                    end loop;
                end if;
                
                -- Branch predicted correctly
                if(clear_spec_i = '1') then
                    for i in 0 to IQ_DEPTH-1 loop
                        if (iq_ffs_s(i).valid = '1' ) then
                            iq_ffs_s(i).is_spec <= '0';
                        end if;
                    end loop;
                end if;
            end if;
        end if;
    end process;
    
    -- This register is needed to match the cycle in which ram data and ff data is read
    addr_reg:process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                issue_full_valid_reg_s      <= '0';
                issue_full_addr_reg_s       <= (others => '0');
                issue_partial_valid_reg_s   <= '0';
                issue_partial_addr_reg_s    <= (others => '0');
            elsif (stall_iq_is_i = '0') then
                issue_full_valid_reg_s      <= issue_full_valid_s;
                issue_full_addr_reg_s       <= issue_full_addr_s;
                issue_partial_valid_reg_s   <= issue_partial_valid_s;
                issue_partial_addr_reg_s    <= issue_partial_addr_s;
            end if;
        end if;
    end process;
    
    -- Asynchronous read from IQ ffs
    process(iq_ffs_s, issue_full_valid_reg_s, issue_partial_valid_reg_s, issue_full_addr_reg_s, cdb_valid_i, issue_partial_addr_reg_s, cdb_addr_i)
    begin
            -- Default values
            issue_rs1_addr_o        <= (others => '0');
            issue_rs2_addr_o        <= (others => '0');
            issue_rs1_ready_o       <= '0';
            issue_rs2_ready_o       <= '0';
            spec_instr_o            <= '0';
            
        -- Issue/Read from the Instruction Queue
        if (issue_full_valid_reg_s = '1') then
            issue_rs1_addr_o        <= iq_ffs_s(to_integer(unsigned(issue_full_addr_reg_s))).rs1_addr;
            issue_rs2_addr_o        <= iq_ffs_s(to_integer(unsigned(issue_full_addr_reg_s))).rs2_addr;
            spec_instr_o            <= iq_ffs_s(to_integer(unsigned(issue_full_addr_reg_s))).is_spec;
            
            -- Set operand ready fields if tag is published, else read from IQ
            if(cdb_valid_i = '1' and iq_ffs_s(to_integer(unsigned(issue_full_addr_reg_s))).rs1_addr = cdb_addr_i) then
                issue_rs1_ready_o   <= '1';
            else
                issue_rs1_ready_o   <= iq_ffs_s(to_integer(unsigned(issue_full_addr_reg_s))).rs1_ready;
            end if;
            
            -- Set operand ready fields if tag is published, else read from IQ
            if(cdb_valid_i = '1' and iq_ffs_s(to_integer(unsigned(issue_full_addr_reg_s))).rs2_addr = cdb_addr_i) then
                issue_rs2_ready_o   <= '1';
            else
                issue_rs2_ready_o   <= iq_ffs_s(to_integer(unsigned(issue_full_addr_reg_s))).rs2_ready;
            end if;
            
        elsif (issue_partial_valid_reg_s = '1') then       
            issue_rs1_addr_o        <= iq_ffs_s(to_integer(unsigned(issue_partial_addr_reg_s))).rs1_addr;
            issue_rs2_addr_o        <= iq_ffs_s(to_integer(unsigned(issue_partial_addr_reg_s))).rs2_addr;
            spec_instr_o            <= iq_ffs_s(to_integer(unsigned(issue_partial_addr_reg_s))).is_spec;
            
            -- Set operand ready fields if tag is published, else read from IQ
            if(cdb_valid_i = '1' and iq_ffs_s(to_integer(unsigned(issue_partial_addr_reg_s))).rs1_addr = cdb_addr_i) then
                issue_rs1_ready_o   <= '1';
            else
                issue_rs1_ready_o   <= iq_ffs_s(to_integer(unsigned(issue_partial_addr_reg_s))).rs1_ready;
            end if;
            
            -- Set operand ready fields if tag is published, else read from IQ
            if(cdb_valid_i = '1' and iq_ffs_s(to_integer(unsigned(issue_partial_addr_reg_s))).rs2_addr = cdb_addr_i) then
                issue_rs2_ready_o   <= '1';
            else
                issue_rs2_ready_o   <= iq_ffs_s(to_integer(unsigned(issue_partial_addr_reg_s))).rs2_ready;
            end if;
        end if;
    end process;
    
    -- Generic priority encoders for finding 
    -- 1) free slot for dispatch 
    -- 2) ready instruction with both operands to issue
    -- 3) ready instruction with one  operand  to issue
    process(clk)
        variable idx  : integer;
        variable found: std_logic;
        variable i    : integer;
    begin
        if rising_edge(clk) then
    
            if reset = '0' then
    
                dispatch_ready_s      <= '0';
                dispatch_addr_s       <= (others => '0');
    
                issue_full_valid_s    <= '0';
                issue_full_addr_s     <= (others => '0');
    
                issue_partial_valid_s <= '0';
                issue_partial_addr_s  <= (others => '0');
    
                dispatch_ptr_s <= 0;
                issue_ptr_s    <= 0;
                partial_ptr_s  <= 0;
    
            else
    
            --------------------------------------------------------------------
            -- DISPATCH (round-robin free slot)
            --------------------------------------------------------------------
                idx := -1;
                found := '0';
    
                for k in 0 to IQ_DEPTH-1 loop
                    i := (dispatch_ptr_s + k) mod IQ_DEPTH;
    
                    if (iq_ffs_s(i).valid = '0' and found = '0') then
                        idx := i;
                        found := '1';
                    end if;
                end loop;
    
                if stall_is_i = '0' then
                    if found = '1' then
                        dispatch_ready_s <= '1';
                        dispatch_addr_s  <= std_logic_vector(to_unsigned(idx, IQ_BITS));
                        dispatch_ptr_s   <= (idx + 1) mod IQ_DEPTH;
                    else
                        dispatch_ready_s <= '0';
                    end if;
                end if;
    
            --------------------------------------------------------------------
            -- FULL ISSUE (both operands ready)
            --------------------------------------------------------------------
                idx := -1;
                found := '0';
    
                for k in 1 to IQ_DEPTH-1 loop
                    i := (issue_ptr_s + k) mod IQ_DEPTH;
    
                    if (iq_ffs_s(i).valid = '1' and
                        iq_ffs_s(i).rs1_ready = '1' and
                        iq_ffs_s(i).rs2_ready = '1' and
                        found = '0') then
    
                        idx := i;
                        found := '1';
                    end if;
                end loop;
    
                if stall_iq_is_i = '0' then
                    if found = '1' then
                        issue_full_valid_s <= '1';
                        issue_full_addr_s  <= std_logic_vector(to_unsigned(idx, IQ_BITS));
                        issue_ptr_s        <= (idx) mod IQ_DEPTH;
                    else
                        issue_full_valid_s <= '0';
                    end if;
                end if;
    
            --------------------------------------------------------------------
            -- PARTIAL ISSUE (one operand ready, no JALR)
            --------------------------------------------------------------------
                idx := -1;
                found := '0';
    
                for k in 1 to IQ_DEPTH-1 loop
                    i := (partial_ptr_s + k) mod IQ_DEPTH;
    
                    if (iq_ffs_s(i).valid = '1' and
                        iq_ffs_s(i).is_jalr = '0' and
                        ((iq_ffs_s(i).rs1_ready = '1' and iq_ffs_s(i).rs2_ready = '0') or
                         (iq_ffs_s(i).rs1_ready = '0' and iq_ffs_s(i).rs2_ready = '1')) and
                        found = '0') then
    
                        idx := i;
                        found := '1';
                    end if;
                end loop;
    
                if stall_iq_is_i = '0' then
                    if found = '1' then
                        issue_partial_valid_s <= '1';
                        issue_partial_addr_s  <= std_logic_vector(to_unsigned(idx, IQ_BITS));
                        partial_ptr_s         <= (idx) mod IQ_DEPTH;
                    else
                        issue_partial_valid_s <= '0';
                    end if;
                end if;
    
            end if;
        end if;
    end process;
    
    
end Behavioral;

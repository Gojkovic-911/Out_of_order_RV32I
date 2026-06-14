library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity data_path is
    generic (
        DATA_WIDTH      : natural  := 32;   -- Data width
        MEM_BYTES       : natural  := 4;    -- (DATA_WIDTH / 8);
        ARCH_ADDR_BITS  : natural  := 5;
        NUM_PHYS_REGS   : natural  := 64;   -- Number of physiscal registers
        PHYS_ADDR_BITS  : natural  := 6;    -- log2(NUM_PHYS_REGS)
        ROB_DEPTH       : natural  := 64;   -- ROB depth of entries
        ROB_ADDR_BITS   : natural  := 6;
        IQ_DEPTH        : natural  := 16;   -- Instruction queue depth
        IQ_BITS         : natural  := 4;    -- log2(IQ_DEPTH)
        RS_DEPTH        : natural  := 16;
        RS_BITS         : natural  := 4
        );
    
    port(
        clk                     : in  std_logic;
        reset                   : in  std_logic;
        
        -- ********* Instruction Memory Interface (read-only) *********
        instr_mem_addr_o        : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Address of instruction
        instr_mem_data_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- Instruction read from memory
        instruction_o           : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Forwarded instruction
        
        -- ********* Data Memory Interface (read/write) *********
        data_mem_addr_o         : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Address for data access
        data_mem_wdata_o        : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data to be written to memory
        data_mem_rdata_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data read from memory
        data_mem_be_o           : out std_logic_vector(MEM_BYTES-1 downto 0);   -- Byte enable (load/store strobe)
        
        -- ********* Control signals ************************
        decode_instr_format_i   : in  std_logic_vector(2 downto 0);
        fetch_is_jump_i        : in  std_logic;
        fetch_is_spec_i        : in  std_logic;
        
        rename_rd_we_i           : in  std_logic;
        rename_rs1_used_i        : in  std_logic;
        rename_rs2_used_i        : in  std_logic;
        rename_snapshot_i        : in  std_logic;

        dispatch_instr_type_i    : in std_logic_vector(3 downto 0);
        dispatch_instr_subtype_i : in std_logic_vector(4 downto 0);
     
        stall_if_i              : in std_logic;     -- Stall fetch stage
        stall_id_i              : in std_logic;     -- Stall decode stage
        stall_rn_i              : in std_logic;     -- Stall rename stage
        stall_is_i              : in std_logic;     -- Stall dispatch/issue stage
        stall_ex_i              : in std_logic;     -- Stall execute stage
        stall_iq_is_i           : in std_logic;     -- Stall issue from IQ
        
        -- if_id_flush_i           : in std_logic;
        pc_next_sel_i           : in std_logic;
        flush_pipe_i            : in std_logic;
        if_id_flush_i           : in std_logic;
        
        -- *********** Status signals ***********************
        rob_full_o              : out std_logic;    -- ROB full
        free_list_fifo_empty_o  : out std_logic;    -- No free registers in fifo
        iq_full_o               : out std_logic;    -- IQ full
        
        misspredict_o           : out std_logic;
        
        rs_full_o               : out std_logic    -- reservation stations full
        );

end entity;

architecture Behavioral of data_path is

    -- Debug
    signal rename_instruction           : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal dispatch_instruction         : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_instruction            : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal execute_instruction          : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Fetch signals
    signal fetch_instruction_s          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal fetch_instr_valid_s          : std_logic;
    signal pc_adder_s                   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal pc_reg_s, pc_next_s          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal pc_revert_addr_s             : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Decode signals
    signal decode_instruction_s         : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal decode_imm_s                 : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal decode_pc_reg_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal decode_instr_valid_s         : std_logic;
    signal decode_spec_instr_s          : std_logic;
    signal decode_is_jump_s             : std_logic;
    
    -- Renaming signals
    signal rename_rs1_arch_addr_s       : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rename_rs2_arch_addr_s       : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rename_rd_arch_addr_s        : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    
    signal rename_rs1_phys_addr_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rename_rs2_phys_addr_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rename_rd_phys_addr_s        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    signal rename_rs1_ready_s           : std_logic;
    signal rename_rs2_ready_s           : std_logic;
    
    signal rename_instr_valid_s         : std_logic;
    signal rename_spec_instr_s          : std_logic;
    
    signal rename_imm_s                 : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rename_pc_reg_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal free_list_fifo_empty_s       : std_logic;


    -- ROB signals
    signal rob_rename_rd_arch_s         : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rob_rename_rd_phys_s         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_rename_prev_phys_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_rename_rd_instr_s        : std_logic;
    signal rob_rename_tail_idx_s        : std_logic_vector(ROB_ADDR_BITS-1 downto 0);    
    signal rob_branch_idx_s             : std_logic_vector(ROB_ADDR_BITS-1 downto 0);   
    
    signal rob_full_s                   : std_logic;
    signal rob_empty_s                  : std_logic;
    
    signal wb_rob_idx_s                 : std_logic_vector(ROB_ADDR_BITS-1 downto 0);

    signal rob_commit_valid_s           : std_logic;
    signal rob_commit_rd_arch_s         : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rob_commit_rd_phys_s         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_commit_prev_phys_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_commit_rd_instr_s        : std_logic;
    
    signal rob_branch_addr_valid_s      : std_logic;
    signal rob_branch_addr_s            : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    
    -- Dispatch/Issue signals
    signal dispatch_valid_s             : std_logic;

    signal dispatch_rs1_addr_s          : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal dispatch_rs2_addr_s          : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal dispatch_rs1_ready_s         : std_logic;
    signal dispatch_rs2_ready_s         : std_logic;
    signal dispatch_rd_addr_s           : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal dispatch_imm_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal dispatch_pc_reg_s            : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal dispatch_rob_idx_s           : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    signal dispatch_spec_instr_s        : std_logic;

    signal issue_rs1_addr_s             : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal issue_rs2_addr_s             : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal issue_rd_addr_s              : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal issue_spec_instr_s           : std_logic;

    signal issue_rs1_data_s             : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_rs2_data_s             : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal issue_imm_s                  : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_pc_reg_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_rob_idx_s              : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    
    signal issue_rs1_ready_s            : std_logic;
    signal issue_rs2_ready_s            : std_logic;
    
    signal issue_instr_type_s           : std_logic_vector(3 downto 0);
    signal issue_instr_subtype_s        : std_logic_vector(4 downto 0);
    
    signal issue_valid_s                : std_logic;
    
    -- Execute signals
    signal execute_valid_s         : std_logic;
    signal execute_rs1_data_s      : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal execute_rs2_data_s      : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal execute_rs1_addr_s      : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal execute_rs2_addr_s      : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal execute_rd_addr_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal execute_rob_idx_s       : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    
    signal execute_instr_type_s    : std_logic_vector(3 downto 0);
    signal execute_instr_subtype_s : std_logic_vector(4 downto 0);
    signal execute_spec_instr_s    : std_logic;
    
    signal execute_rs1_ready_s     : std_logic;
    signal execute_rs2_ready_s     : std_logic;
    
    signal execute_imm_s           : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal execute_pc_reg_s        : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal cdb_valid_s             : std_logic;
    signal cdb_data_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal cdb_rd_addr_s           : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    signal branch_taken_s          : std_logic;
    signal branch_taken_valid_s    : std_logic;
    signal branch_taken_idx_s      : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    
    type phys_reg_array_t is array (0 to NUM_PHYS_REGS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal phys_regs_s : phys_reg_array_t := (others => (others => '0'));
    
    signal spec_reg_s, spec_lock    : std_logic;
    signal clear_spec_s             : std_logic;
    signal pc_halt_s, halt_lock_s   : std_logic;
    signal pc_reg_s_delayed         : std_logic_vector(DATA_WIDTH-1 downto 0);
    
begin
    
    -- Instruction memory
    fetch_instruction_s <= instr_mem_data_i;

    -- Control signals
    instruction_o       <= fetch_instruction_s;
    rob_full_o          <= rob_full_s;
    
    -- Physical registers bank
    -- Synchronous write
    phys_regs_write:process(clk)
    begin
        if rising_edge(clk) then
            if(reset = '0') then
                phys_regs_s <= (others => (others => '0'));
            elsif cdb_valid_s = '1' then -- cdb_valid ?
                if(to_integer(unsigned(cdb_rd_addr_s)) /= 0) then
                    phys_regs_s(to_integer(unsigned(cdb_rd_addr_s))) <= cdb_data_s;
                end if;
            end if;
        end if;
    end process;
    
    -- Asynchronous read
    phys_regs_read : process (issue_rs1_addr_s, issue_rs2_addr_s, phys_regs_s) is
    begin
    
        if(to_integer(unsigned(issue_rs1_addr_s)) = 0) then
            issue_rs1_data_s <= std_logic_vector(to_unsigned(0, DATA_WIDTH));
        else
            if(cdb_valid_s = '1' and cdb_rd_addr_s = issue_rs1_addr_s ) then
                issue_rs1_data_s <= cdb_data_s;
            else
                issue_rs1_data_s <= phys_regs_s(to_integer(unsigned(issue_rs1_addr_s)));
            end if;
        end if;
        
        if(to_integer(unsigned(issue_rs2_addr_s)) = 0) then
            issue_rs2_data_s <= std_logic_vector(to_unsigned(0, DATA_WIDTH));
        else
            if(cdb_valid_s = '1' and cdb_rd_addr_s = issue_rs2_addr_s ) then
                issue_rs2_data_s <= cdb_data_s;
            else
                issue_rs2_data_s <= phys_regs_s(to_integer(unsigned(issue_rs2_addr_s)));
            end if;
        end if;
    end process;
    
    --*********** INSTRUCTION FETCH STAGE *********************
    -- Program counter
    pc_reg : process (clk) is
    begin
        if rising_edge(clk) then
            if reset = '0' then
                pc_reg_s <= (others => '0');
                fetch_instr_valid_s <= '0';
            elsif flush_pipe_i = '1' then      
                pc_reg_s   <= pc_next_s;
                fetch_instr_valid_s <= '1';
            elsif decode_is_jump_s = '1'  then -- BRANCH and JAL
                pc_reg_s <= std_logic_vector(unsigned(decode_pc_reg_s) + unsigned(decode_imm_s) + to_unsigned(4, DATA_WIDTH));
                fetch_instr_valid_s <= '1';
            elsif stall_if_i = '0' then -- stall
                pc_reg_s   <= pc_next_s;
                fetch_instr_valid_s <= '1';
            end if;
        end if;
    end process;
    
    -- PC gets either pc+4 or pc+imm
    pc_add:process (pc_reg_s, decode_imm_s, decode_pc_reg_s, decode_is_jump_s) begin
        if (decode_is_jump_s = '1') then -- BRANCH and JAL
            instr_mem_addr_o <= std_logic_vector(unsigned(decode_pc_reg_s) + unsigned(decode_imm_s));
        else
            instr_mem_addr_o <= pc_reg_s;
        end if;
    end process;
    
    pc_adder_s  <= std_logic_vector(unsigned(pc_reg_s) + to_unsigned(4, DATA_WIDTH));
            
    -- MUX selecting the next addr for PC
    with pc_next_sel_i select
       pc_next_s <= 
       pc_adder_s       when '0',       -- PC+4
       pc_revert_addr_s when others;    -- jump addr
    
    -- Register for marking instructions speculative - JALR & BRANCH
    spec_reg : process (clk) is
    begin
       if rising_edge(clk) then
            if reset = '0' then
                spec_reg_s  <= '0';
                spec_lock   <= '0';
            else
                if((flush_pipe_i = '1' or clear_spec_s = '1') and spec_lock = '1') then
                    spec_reg_s  <= '0';
                    spec_lock   <= '0';
                elsif(fetch_is_spec_i = '1' and spec_lock = '0') and stall_if_i = '0' then
                    spec_reg_s  <= '1';
                    spec_lock   <= '1';
                end if;
          end if;
       end if;
    end process;
    
    --*********************************************************
    
    --*********** INSTRUCTION DECODE STAGE ********************   
    --IF/ID register
    IF_ID : process (clk) is
    begin
        if rising_edge(clk) then
            if reset = '0' or flush_pipe_i = '1' then --  or if_id_flush_i = '1' 
                decode_is_jump_s        <= '0';
                decode_instruction_s    <= (others => '0');
                decode_pc_reg_s         <= (others => '0');
                decode_spec_instr_s     <= '0';
                decode_instr_valid_s    <= '0';
                
            elsif stall_id_i = '0'then 
                decode_is_jump_s        <= fetch_is_jump_i;
                decode_instruction_s    <= fetch_instruction_s;
                decode_spec_instr_s     <= spec_reg_s and (not clear_spec_s);
                
                if (decode_is_jump_s = '1') then -- BRANCH and JAL
                    decode_pc_reg_s <= std_logic_vector(unsigned(decode_pc_reg_s) + unsigned(decode_imm_s));
                else
                    decode_pc_reg_s     <= pc_reg_s;
                end if;
                if fetch_instruction_s = std_logic_vector(to_unsigned(0, DATA_WIDTH)) then
                    decode_instr_valid_s <= '0';
                else  
                    decode_instr_valid_s <= fetch_instr_valid_s;
                end if;
            end if;
        end if;
    end process;
    
    -- Module for extracting/extending the immediate
    immediate_1 : entity work.immediate
         port map (
            instr_i         => decode_instruction_s,
            instr_format_i  => decode_instr_format_i,
            imm_extended_o  => decode_imm_s
            );
    
    --********************************************************
    
    --*********** REGISTER RENAMING STAGE ********************
    --ID/RN register
    ID_RN : process (clk) is
    begin
        if rising_edge(clk) then
            if reset = '0' or flush_pipe_i = '1' then
                rename_rs1_arch_addr_s  <= (others => '0');
                rename_rs2_arch_addr_s  <= (others => '0');
                rename_rd_arch_addr_s   <= (others => '0');
                rename_imm_s            <= (others => '0');
                rename_pc_reg_s         <= (others => '0');
                rename_instruction      <= (others => '0');
                rename_instr_valid_s    <= '0';
                rename_spec_instr_s     <= '0';
                
            elsif stall_rn_i = '0' then
                rename_rs1_arch_addr_s  <= decode_instruction_s(19 downto 15);
                rename_rs2_arch_addr_s  <= decode_instruction_s(24 downto 20);
                rename_rd_arch_addr_s   <= decode_instruction_s(11 downto 7);
                rename_imm_s            <= decode_imm_s;
                rename_pc_reg_s         <= decode_pc_reg_s;
                rename_instruction      <= decode_instruction_s;
                rename_instr_valid_s    <= decode_instr_valid_s;
                rename_spec_instr_s     <= decode_spec_instr_s and (not clear_spec_s);
            end if;
        end if;
    end process;
    
    u_renaming_module: entity work.renaming_module
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            NUM_PHYS_REGS   => NUM_PHYS_REGS,
            ARCH_ADDR_BITS  => ARCH_ADDR_BITS,
            PHYS_ADDR_BITS  => PHYS_ADDR_BITS,
            ROB_DEPTH       => ROB_DEPTH,
            ROB_ADDR_BITS   => ROB_ADDR_BITS    
        )
        port map (
            clk                     => clk,
            reset                   => reset,
    
            rs1_arch_addr_i         => rename_rs1_arch_addr_s,
            rs2_arch_addr_i         => rename_rs2_arch_addr_s,
            rd_arch_addr_i          => rename_rd_arch_addr_s,
            
            rename_rs1_used_i       => rename_rs1_used_i,
            rename_rs2_used_i       => rename_rs2_used_i,
            rd_we_i                 => rename_rd_we_i,
            rename_snapshot_i       => rename_snapshot_i,
            flush_i                 => flush_pipe_i,
            
            rename_instr_valid_i    => rename_instr_valid_s and (not stall_rn_i), -- ?? Can I have a scenario where i use stall and not instr_valid ..
    
            cdb_valid_i             => cdb_valid_s, 
            cdb_rd_addr_i           => cdb_rd_addr_s,
    
            rs1_phys_addr_o         => rename_rs1_phys_addr_s,
            rs2_phys_addr_o         => rename_rs2_phys_addr_s,
            rd_phys_addr_o          => rename_rd_phys_addr_s,
    
            rs1_ready_o             => rename_rs1_ready_s,
            rs2_ready_o             => rename_rs2_ready_s,
    
            rob_rename_rd_arch_o    => rob_rename_rd_arch_s,
            rob_rename_rd_phys_o    => rob_rename_rd_phys_s,
            rob_rename_prev_phys_o  => rob_rename_prev_phys_s,
            rob_rename_rd_instr_o   => rob_rename_rd_instr_s,
            
            rob_commit_rd_arch_i    => rob_commit_rd_arch_s,
            rob_commit_rd_phys_i    => rob_commit_rd_phys_s,
            rob_commit_prev_phys_i  => rob_commit_prev_phys_s,
            rob_commit_valid_i      => rob_commit_valid_s,
            rob_commit_rd_instr_i   => rob_commit_rd_instr_s,
            
            free_list_fifo_empty_o  => free_list_fifo_empty_s
            );
        
    free_list_fifo_empty_o  <= free_list_fifo_empty_s;
    
    --********************************************************
    
    --***************** DISPATCH/ISSUE STAGE **************************
    
    --RN/IS register
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '0' or flush_pipe_i = '1' then
                dispatch_rs1_addr_s     <= (others => '0');
                dispatch_rs2_addr_s     <= (others => '0');
                dispatch_rd_addr_s      <= (others => '0');
                
                dispatch_rs1_ready_s    <= '0';
                dispatch_rs2_ready_s    <= '0';
                
                dispatch_pc_reg_s       <= (others => '0');
                
                dispatch_imm_s          <= (others => '0');
                dispatch_valid_s        <= '0';
                dispatch_rob_idx_s      <= (others => '0');
                dispatch_instruction    <= (others => '0');
                dispatch_spec_instr_s   <= '0';
                
            elsif stall_is_i = '0' then
                dispatch_rs1_addr_s     <= rename_rs1_phys_addr_s;
                dispatch_rs2_addr_s     <= rename_rs2_phys_addr_s;
                dispatch_rd_addr_s      <= rename_rd_phys_addr_s;
                
                dispatch_rs1_ready_s    <= rename_rs1_ready_s;
                dispatch_rs2_ready_s    <= rename_rs2_ready_s;
                
                dispatch_pc_reg_s       <= rename_pc_reg_s;
                
                dispatch_imm_s          <= rename_imm_s;
                dispatch_valid_s        <= rename_instr_valid_s and (not stall_rn_i);
                dispatch_rob_idx_s      <= rob_rename_tail_idx_s;
                dispatch_instruction    <= rename_instruction;
                dispatch_spec_instr_s   <=  rename_spec_instr_s and (not clear_spec_s);
            end if;
        end if;
    end process;
    
    u_issue_module : entity work.issue_module
        generic map (
            IQ_DEPTH       => IQ_DEPTH,
            IQ_BITS        => IQ_BITS,
            PHYS_ADDR_BITS => PHYS_ADDR_BITS,
            DATA_WIDTH     => DATA_WIDTH,
            ROB_ADDR_BITS   => ROB_ADDR_BITS    
        )
        port map (
            clk                      => clk,
            reset                    => reset,
            
            dispatch_valid_i         => dispatch_valid_s and (not stall_is_i), 
            dispatch_rs1_addr_i      => dispatch_rs1_addr_s,
            dispatch_rs2_addr_i      => dispatch_rs2_addr_s,
            dispatch_rd_addr_i       => dispatch_rd_addr_s,
            
            dispatch_rs1_ready_i     => dispatch_rs1_ready_s,
            dispatch_rs2_ready_i     => dispatch_rs2_ready_s,
            
            dispatch_pc_reg_i        => dispatch_pc_reg_s,
            dispatch_imm_i           => dispatch_imm_s,            
            
            dispatch_instr_type_i    => dispatch_instr_type_i,
            dispatch_instr_subtype_i => dispatch_instr_subtype_i,
            
            dispatch_rob_idx_i       => dispatch_rob_idx_s,
            dispatch_instruction     => dispatch_instruction,
            spec_instr_i             => dispatch_spec_instr_s and (not clear_spec_s),
            
            flush_pipe_i             => flush_pipe_i,
            
            cdb_addr_i               => cdb_rd_addr_s,
            cdb_valid_i              => cdb_valid_s,
    
            stall_iq_is_i            => stall_iq_is_i,
            stall_is_i               => stall_is_i,
            clear_spec_i             => clear_spec_s,
            
            issue_valid_o            => issue_valid_s,
            
            issue_rs1_addr_o         => issue_rs1_addr_s,   
            issue_rs2_addr_o         => issue_rs2_addr_s,   
            issue_rd_addr_o          => issue_rd_addr_s,   
            issue_rob_idx_o          => issue_rob_idx_s,    
            
            issue_instr_type_o       => issue_instr_type_s,
            issue_instr_subtype_o    => issue_instr_subtype_s,
            spec_instr_o             => issue_spec_instr_s,
            
            issue_rs1_ready_o        => issue_rs1_ready_s,      
            issue_rs2_ready_o        => issue_rs2_ready_s,      
                
            issue_imm_o              => issue_imm_s,   
            
            issue_pc_reg_o           => issue_pc_reg_s,   
            issue_instruction        => issue_instruction,
        
            iq_full_o                => iq_full_o           
        );

    --********************************************************
    
    --******************** EXECUTE STAGE *********************
    -- ISSUE/EX register
    process (clk)
    begin
        if (rising_edge(clk)) then
            if reset = '0' or flush_pipe_i = '1' then
                execute_valid_s         <= '0';
                execute_rs1_ready_s     <= '0';  
                execute_rs2_ready_s     <= '0';
                execute_rs1_data_s      <= (others => '0');
                execute_rs2_data_s      <= (others => '0');
                execute_rs1_addr_s      <= (others => '0');
                execute_rs2_addr_s      <= (others => '0');
                execute_rd_addr_s       <= (others => '0');
                execute_instr_type_s    <= (others => '0');
                execute_instr_subtype_s <= (others => '0');
                execute_imm_s           <= (others => '0');
                execute_pc_reg_s        <= (others => '0');
                execute_rob_idx_s       <= (others => '0'); 
                execute_instruction     <= (others => '0'); 
                execute_spec_instr_s    <= '0';
                 
            elsif stall_ex_i = '0' then
                execute_valid_s         <= issue_valid_s;
                execute_rs1_ready_s     <= issue_rs1_ready_s;    
                execute_rs2_ready_s     <= issue_rs2_ready_s;  
                execute_rs1_data_s      <= issue_rs1_data_s;
                execute_rs2_data_s      <= issue_rs2_data_s;
                execute_rs1_addr_s      <= issue_rs1_addr_s;
                execute_rs2_addr_s      <= issue_rs2_addr_s;
                execute_rd_addr_s       <= issue_rd_addr_s;
                execute_instr_type_s    <= issue_instr_type_s;
                execute_instr_subtype_s <= issue_instr_subtype_s;
                execute_rob_idx_s       <= issue_rob_idx_s; 
                execute_imm_s           <= issue_imm_s;     
                execute_pc_reg_s        <= issue_pc_reg_s;     
                execute_instruction     <= issue_instruction;
                execute_spec_instr_s    <= issue_spec_instr_s and (not clear_spec_s);
            end if;
        end if;
    end process;
    
    -- Generate the jump addr if jump/branch instruction
    -- Should be one adder
    generate_rob_jump_addr: 
    process(execute_instr_type_s, execute_rs1_data_s, execute_pc_reg_s, execute_imm_s, execute_rob_idx_s)
    begin
        rob_branch_addr_valid_s <= '0';
        rob_branch_addr_s       <= (others => '0');
        rob_branch_idx_s        <= execute_rob_idx_s;
        
        if (execute_instr_type_s = JALR) then
            rob_branch_addr_valid_s <= '1';
            rob_branch_addr_s <= std_logic_vector(unsigned(execute_rs1_data_s) + unsigned(execute_imm_s));
        elsif (execute_instr_type_s = BRANCH) then
            rob_branch_addr_valid_s <= '1';
            rob_branch_addr_s <= std_logic_vector(unsigned(execute_pc_reg_s) + to_unsigned(4, DATA_WIDTH));
        end if;
    end process;
    
    execute_inst : entity work.execute_module
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            PHYS_ADDR_BITS  => PHYS_ADDR_BITS,
            RS_DEPTH        => RS_DEPTH,
            RS_BITS         => RS_BITS,
            MEM_BYTES       => MEM_BYTES,
            ROB_ADDR_BITS   => ROB_ADDR_BITS    
        )
        port map (
            clk                 => clk,
            reset               => reset,
            
            execute_valid_i         => execute_valid_s and (not stall_ex_i),
            
            execute_rs1_addr_i      => execute_rs1_addr_s,
            execute_rs2_addr_i      => execute_rs2_addr_s,
            execute_rs1_data_i      => execute_rs1_data_s,
            execute_rs2_data_i      => execute_rs2_data_s,
            execute_rd_addr_i       => execute_rd_addr_s,
            execute_rob_idx_i       => execute_rob_idx_s,
            
            execute_instr_type_i    => execute_instr_type_s,
            execute_instr_subtype_i => execute_instr_subtype_s,
            execute_spec_instr_i    => execute_spec_instr_s and (not clear_spec_s),
            flush_pipe_i            => flush_pipe_i,
            
            execute_rs1_ready_i     => execute_rs1_ready_s,
            execute_rs2_ready_i     => execute_rs2_ready_s,
            
            execute_imm_i           => execute_imm_s,
            
            execute_pc_reg_i        => execute_pc_reg_s,
            clear_spec_i            => clear_spec_s,
            
            rs_full_o               => rs_full_o,
            branch_taken_o          => branch_taken_s,
            branch_taken_valid_o    => branch_taken_valid_s,
            branch_taken_idx_o      => branch_taken_idx_s,
            
            cdb_valid_o             => cdb_valid_s,
            cdb_data_o              => cdb_data_s,
            cdb_addr_o              => cdb_rd_addr_s,
            wb_rob_idx_o            => wb_rob_idx_s,
            
            data_mem_addr_o         => data_mem_addr_o,
            data_mem_wdata_o        => data_mem_wdata_o,
            data_mem_rdata_i        => data_mem_rdata_i,
            data_mem_be_o           => data_mem_be_o,
            instruction             => execute_instruction
        );
    
    --********************************************************
    
    --******************** COMMIT STAGE *********************
    -- ROB
    u_rob : entity work.ROB
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            ROB_DEPTH       => ROB_DEPTH,
            ROB_ADDR_BITS   => ROB_ADDR_BITS,
            ARCH_ADDR_BITS  => ARCH_ADDR_BITS,
            NUM_PHYS_REGS   => NUM_PHYS_REGS,
            PHYS_ADDR_BITS  => PHYS_ADDR_BITS
        )
        port map (
            clk                     => clk,
            reset                   => reset,
            
            -- Rename
            rename_instr_valid_i    => rename_instr_valid_s and (not stall_rn_i) and (not flush_pipe_i),   
            
            rename_rd_arch_i        => rob_rename_rd_arch_s,   
            rename_rd_phys_i        => rob_rename_rd_phys_s,   
            rename_prev_phys_i      => rob_rename_prev_phys_s,
            rename_rd_instr_i       => rob_rename_rd_instr_s, 
            rob_tail_idx_o          => rob_rename_tail_idx_s,    
            rename_spec_instr_i     => rename_spec_instr_s and (not clear_spec_s),
            rename_instruction_i    => rename_instruction,
            
            -- WB
            wb_valid_i              => cdb_valid_s,
            wb_rob_idx_i            => wb_rob_idx_s,
            
            -- Branch addr port
            branch_addr_i           => rob_branch_addr_s,
            branch_addr_valid_i     => rob_branch_addr_valid_s,
            branch_addr_tag_i       => rob_branch_idx_s,
            
            branch_taken_i          => branch_taken_s,
            branch_taken_valid_i    => branch_taken_valid_s,
            branch_taken_idx_i      => branch_taken_idx_s,
            
            -- Commit
            commit_valid_o          => rob_commit_valid_s,
            commit_rd_arch_o        => rob_commit_rd_arch_s,
            commit_rd_phys_o        => rob_commit_rd_phys_s,
            commit_prev_phys_o      => rob_commit_prev_phys_s,
            commit_rd_instr_o       => rob_commit_rd_instr_s,
            
            commit_jump_addr_o      => pc_revert_addr_s,
            misspredict_o           => misspredict_o,
            clear_spec_o            => clear_spec_s,
            
            rob_full_o              => rob_full_s,
            rob_empty_o             => rob_empty_s
        );
    
    --********************************************************
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity data_path is
    generic (
        DATA_WIDTH      : natural  := 32;
        MEM_BYTES       : natural  := 4;
        ARCH_ADDR_BITS  : natural  := 5;
        NUM_PHYS_REGS   : natural  := 64;
        PHYS_ADDR_BITS  : natural  := 6;
        ROB_DEPTH       : natural  := 64;
        ROB_ADDR_BITS   : natural  := 6;
        IQ_DEPTH        : natural  := 16;
        IQ_BITS         : natural  := 4;
        LSQ_DEPTH       : natural  := 16;
        LSQ_BITS        : natural  := 4;
        RF_RAM_STYLE    : string   := "auto";
        IQ_RAM_STYLE    : string   := "distributed"
        );
    
    port(
        clk                     : in  std_logic;
        reset                   : in  std_logic;
        
        -- Instruction memory port
        instr_mem_addr_o        : out std_logic_vector(DATA_WIDTH-1 downto 0);
        instr_mem_data_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        instruction_o           : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        -- Data memory port
        data_mem_req_o          : out std_logic;
        data_mem_ready_i        : in  std_logic;
        data_mem_we_o           : out std_logic;
        data_mem_rsp_i          : in  std_logic;
        data_mem_addr_o         : out std_logic_vector(DATA_WIDTH-1 downto 0);
        data_mem_wdata_o        : out std_logic_vector(DATA_WIDTH-1 downto 0);
        data_mem_rdata_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        data_mem_be_o           : out std_logic_vector(MEM_BYTES-1 downto 0);
        
        -- Control signals
        decode_instr_format_i   : in  std_logic_vector(2 downto 0);
        fetch_is_jump_i        : in  std_logic;
        fetch_is_jalr_i        : in  std_logic;
        
        rename_rd_we_i           : in  std_logic;
        rename_rs1_used_i        : in  std_logic;
        rename_rs2_used_i        : in  std_logic;

        dispatch_instr_type_i    : in std_logic_vector(3 downto 0);
        dispatch_instr_subtype_i : in std_logic_vector(4 downto 0);
     
        stall_if_i              : in std_logic;
        stall_id_i              : in std_logic;
        stall_rn_i              : in std_logic;
        stall_is_i              : in std_logic;
        stall_ex_i              : in std_logic;
        stall_iq_is_i           : in std_logic;
        
        pc_next_sel_i           : in std_logic;
        flush_pipe_i            : in std_logic;
        if_id_flush_i           : in std_logic;
        
        -- Status signals
        rob_full_o              : out std_logic;
        free_list_fifo_empty_o  : out std_logic;
        iq_full_o               : out std_logic;
        
        misspredict_o           : out std_logic;
        
        ex_busy_o               : out std_logic;
        lsq_full_o              : out std_logic
        );

end entity;

architecture Behavioral of data_path is

    -- Debug outputs
    signal rename_instruction           : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal dispatch_instruction         : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_instruction            : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal execute_instruction          : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Fetch signals
    signal fetch_instruction_s          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal fetch_instr_valid_s          : std_logic;
    signal fetch_pc_s                   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal fetch_addr_s                 : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal fetch_ok_s                   : std_logic;
    signal jump_redirect_s              : std_logic;
    signal pc_reg_s                     : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal pc_revert_addr_s             : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Decode signals
    signal decode_instruction_s         : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal decode_imm_s                 : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal decode_pc_reg_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal decode_instr_valid_s         : std_logic;
    signal decode_is_jump_s             : std_logic;
    signal decode_is_jalr_s             : std_logic;
    signal jalr_wait_s                  : std_logic;
    signal jalr_redirect_s              : std_logic;
    signal jalr_redirect_addr_s         : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Rename signals
    signal rename_rs1_arch_addr_s       : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rename_rs2_arch_addr_s       : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rename_rd_arch_addr_s        : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    
    signal rename_rs1_phys_addr_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rename_rs2_phys_addr_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rename_rd_phys_addr_s        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    signal rename_rs1_ready_s           : std_logic;
    signal rename_rs2_ready_s           : std_logic;
    
    signal rename_instr_valid_s         : std_logic;
    
    signal rename_imm_s                 : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rename_pc_reg_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal free_list_fifo_empty_s       : std_logic;

    -- ROB signals
    signal rob_rename_rd_arch_s         : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rob_rename_rd_phys_s         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_rename_prev_phys_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_rename_rd_instr_s        : std_logic;
    signal rob_rename_tail_idx_s        : std_logic_vector(ROB_ADDR_BITS-1 downto 0);    
    signal rob_commit_store_s           : std_logic;
    signal store_done_valid_s           : std_logic;
    signal store_done_idx_s             : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    
    -- LSQ signals
    signal rename_is_mem_s              : std_logic;
    signal rename_is_store_s            : std_logic;
    signal lsq_alloc_valid_s            : std_logic;
    signal lsq_tail_idx_s               : std_logic_vector(LSQ_BITS-1 downto 0);
    signal lsq_full_s                   : std_logic;
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
    
    -- Dispatch/issue signals
    signal dispatch_valid_s             : std_logic;

    signal dispatch_rs1_addr_s          : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal dispatch_rs2_addr_s          : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal dispatch_rs1_ready_s         : std_logic;
    signal dispatch_rs2_ready_s         : std_logic;
    signal dispatch_rd_addr_s           : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal dispatch_imm_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal dispatch_pc_reg_s            : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal dispatch_rob_idx_s           : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    signal dispatch_lsq_idx_s           : std_logic_vector(LSQ_BITS-1 downto 0);

    signal issue_rs1_addr_s             : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal issue_rs2_addr_s             : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal issue_rd_addr_s              : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);

    signal issue_rs1_data_s             : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_rs2_data_s             : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal issue_imm_s                  : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_pc_reg_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal issue_rob_idx_s              : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    signal issue_lsq_idx_s              : std_logic_vector(LSQ_BITS-1 downto 0);
    
    
    signal issue_instr_type_s           : std_logic_vector(3 downto 0);
    signal issue_instr_subtype_s        : std_logic_vector(4 downto 0);
    
    signal issue_valid_s                : std_logic;
    
    -- Execute signals
    signal ex_valid_s                   : std_logic;
    signal alu_valid_s                  : std_logic;
    signal alu_op_a_s                   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_op_b_s                   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal branch_valid_s               : std_logic;
    signal lsq_fill_valid_s             : std_logic;
    signal lsq_fill_addr_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_cdb_req_s                : std_logic;
    signal alu_cdb_grant_s              : std_logic;
    signal alu_cdb_data_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_cdb_rd_addr_s            : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal alu_cdb_rob_idx_s            : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    signal lsq_cdb_req_s                : std_logic;
    signal lsq_cdb_grant_s              : std_logic;
    signal lsq_cdb_data_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal lsq_cdb_rd_addr_s            : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal lsq_cdb_rob_idx_s            : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    signal execute_valid_s         : std_logic;
    signal execute_rs1_data_s      : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal execute_rs2_data_s      : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal execute_rs1_addr_s      : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal execute_rs2_addr_s      : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal execute_rd_addr_s       : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal execute_rob_idx_s       : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    signal execute_lsq_idx_s       : std_logic_vector(LSQ_BITS-1 downto 0);
    
    signal execute_instr_type_s    : std_logic_vector(3 downto 0);
    signal execute_instr_subtype_s : std_logic_vector(4 downto 0);
    
    
    signal execute_imm_s           : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal execute_pc_reg_s        : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal cdb_valid_s             : std_logic;
    signal cdb_data_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal cdb_rd_addr_s           : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    signal branch_taken_s          : std_logic;
    signal branch_taken_valid_s    : std_logic;
    signal branch_taken_idx_s      : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    signal ex_busy_s               : std_logic;
    
    type phys_reg_array_t is array (0 to NUM_PHYS_REGS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal phys_regs_s : phys_reg_array_t := (others => (others => '0'));
    attribute ram_style : string;
    attribute ram_style of phys_regs_s : signal is RF_RAM_STYLE;
    
    signal pc_halt_s, halt_lock_s   : std_logic;
    signal pc_reg_s_delayed         : std_logic_vector(DATA_WIDTH-1 downto 0);
    
begin
    
    -- Instruction memory port
    fetch_instruction_s <= instr_mem_data_i;

    instruction_o       <= fetch_instruction_s when fetch_ok_s = '1' else (others => '0');
    rob_full_o          <= rob_full_s;
    
    -- Physical register file
    phys_regs_write:process(clk)
    begin
        if rising_edge(clk) then
            if cdb_valid_s = '1' then
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
            issue_rs1_data_s <= phys_regs_s(to_integer(unsigned(issue_rs1_addr_s)));
        end if;
        
        if(to_integer(unsigned(issue_rs2_addr_s)) = 0) then
            issue_rs2_data_s <= std_logic_vector(to_unsigned(0, DATA_WIDTH));
        else
            issue_rs2_data_s <= phys_regs_s(to_integer(unsigned(issue_rs2_addr_s)));
        end if;
    end process;
    
    -- Fetch stage
    
    jump_redirect_s <= decode_is_jump_s and (not stall_id_i);
    
    fetch_ok_s      <= fetch_instr_valid_s and (not jump_redirect_s) and (not decode_is_jalr_s) and (not jalr_wait_s);
    
    -- Next fetch address
    fetch_addr:process (flush_pipe_i, pc_next_sel_i, pc_revert_addr_s, jalr_redirect_s, jalr_redirect_addr_s,
                        jump_redirect_s, decode_pc_reg_s, decode_imm_s, decode_is_jalr_s, jalr_wait_s,
                        stall_if_i, fetch_pc_s, pc_reg_s) begin
        if (flush_pipe_i = '1' and pc_next_sel_i = '1') then
            fetch_addr_s <= pc_revert_addr_s;
        elsif (jalr_redirect_s = '1') then
            fetch_addr_s <= jalr_redirect_addr_s;
        elsif (decode_is_jalr_s = '1' or jalr_wait_s = '1') then
            fetch_addr_s <= fetch_pc_s;
        elsif (jump_redirect_s = '1') then
            fetch_addr_s <= std_logic_vector(unsigned(decode_pc_reg_s) + unsigned(decode_imm_s));
        elsif (stall_if_i = '1') then
            fetch_addr_s <= fetch_pc_s;
        else
            fetch_addr_s <= pc_reg_s;
        end if;
    end process;
    
    instr_mem_addr_o <= fetch_addr_s;
    
    -- Program counter
    pc_reg : process (clk) is
    begin
        if rising_edge(clk) then
            if reset = '0' then
                pc_reg_s            <= (others => '0');
                fetch_pc_s          <= (others => '0');
                fetch_instr_valid_s <= '0';
            elsif (flush_pipe_i = '1' or jalr_redirect_s = '1' or jump_redirect_s = '1' or stall_if_i = '0') then
                fetch_pc_s          <= fetch_addr_s;
                fetch_instr_valid_s <= '1';
                pc_reg_s            <= std_logic_vector(unsigned(fetch_addr_s) + to_unsigned(4, DATA_WIDTH));
            end if;
        end if;
    end process;
    
    -- Decode stage, IF/ID register
    IF_ID : process (clk) is
    begin
        if rising_edge(clk) then
            if reset = '0' or flush_pipe_i = '1' then
                decode_is_jump_s        <= '0';
                decode_is_jalr_s        <= '0';
                decode_instruction_s    <= (others => '0');
                decode_pc_reg_s         <= (others => '0');
                decode_instr_valid_s    <= '0';
                
            elsif stall_id_i = '0'then 
                if (fetch_ok_s = '1') then
                    decode_is_jump_s        <= fetch_is_jump_i;
                    decode_is_jalr_s        <= fetch_is_jalr_i;
                    decode_instruction_s    <= fetch_instruction_s;
                    decode_pc_reg_s         <= fetch_pc_s;
                    if fetch_instruction_s = std_logic_vector(to_unsigned(0, DATA_WIDTH)) then
                        decode_instr_valid_s <= '0';
                    else  
                        decode_instr_valid_s <= '1';
                    end if;
                else
                    decode_is_jump_s        <= '0';
                    decode_is_jalr_s        <= '0';
                    decode_instruction_s    <= (others => '0');
                    decode_instr_valid_s    <= '0';
                end if;
            end if;
        end if;
    end process;
    
    -- Immediate extraction
    immediate_1 : entity work.immediate
         port map (
            instr_i         => decode_instruction_s,
            instr_format_i  => decode_instr_format_i,
            imm_extended_o  => decode_imm_s
            );
    
    -- Rename stage, ID/RN register
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
                
            elsif stall_rn_i = '0' then
                rename_rs1_arch_addr_s  <= decode_instruction_s(19 downto 15);
                rename_rs2_arch_addr_s  <= decode_instruction_s(24 downto 20);
                rename_rd_arch_addr_s   <= decode_instruction_s(11 downto 7);
                rename_imm_s            <= decode_imm_s;
                rename_pc_reg_s         <= decode_pc_reg_s;
                rename_instruction      <= decode_instruction_s;
                rename_instr_valid_s    <= decode_instr_valid_s;
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
            flush_i                 => flush_pipe_i,
            
            rename_instr_valid_i    => rename_instr_valid_s and (not stall_rn_i),
    
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
    
    -- Issue stage, RN/IS register
    
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
                dispatch_lsq_idx_s      <= (others => '0');
                dispatch_instruction    <= (others => '0');
                
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
                dispatch_lsq_idx_s      <= lsq_tail_idx_s;
                dispatch_instruction    <= rename_instruction;
            else
                -- Stalled: keep snooping CDB
                if (cdb_valid_s = '1' and cdb_rd_addr_s = dispatch_rs1_addr_s) then
                    dispatch_rs1_ready_s    <= '1';
                end if;
                if (cdb_valid_s = '1' and cdb_rd_addr_s = dispatch_rs2_addr_s) then
                    dispatch_rs2_ready_s    <= '1';
                end if;
            end if;
        end if;
    end process;
    
    u_issue_module : entity work.issue_module
        generic map (
            IQ_DEPTH       => IQ_DEPTH,
            IQ_BITS        => IQ_BITS,
            PHYS_ADDR_BITS => PHYS_ADDR_BITS,
            DATA_WIDTH     => DATA_WIDTH,
            ROB_ADDR_BITS   => ROB_ADDR_BITS,
            LSQ_BITS        => LSQ_BITS,
            PAYLOAD_RAM_STYLE => IQ_RAM_STYLE
        )
        port map (
            clk                      => clk,
            reset                    => reset,
            
            dispatch_valid_i         => dispatch_valid_s and (not stall_is_i) and (not flush_pipe_i), 
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
            dispatch_lsq_idx_i       => dispatch_lsq_idx_s,
            dispatch_instruction     => dispatch_instruction,
            
            flush_pipe_i             => flush_pipe_i,
            
            cdb_addr_i               => cdb_rd_addr_s,
            cdb_valid_i              => cdb_valid_s,
    
            stall_iq_is_i            => stall_iq_is_i,
            stall_is_i               => stall_is_i,
            
            issue_valid_o            => issue_valid_s,
            
            issue_rs1_addr_o         => issue_rs1_addr_s,   
            issue_rs2_addr_o         => issue_rs2_addr_s,   
            issue_rd_addr_o          => issue_rd_addr_s,   
            issue_rob_idx_o          => issue_rob_idx_s,    
            issue_lsq_idx_o          => issue_lsq_idx_s,
            
            issue_instr_type_o       => issue_instr_type_s,
            issue_instr_subtype_o    => issue_instr_subtype_s,
            
            issue_imm_o              => issue_imm_s,   
            
            issue_pc_reg_o           => issue_pc_reg_s,   
            issue_instruction        => issue_instruction,
        
            iq_full_o                => iq_full_o           
        );

    -- Execute stage, IS/EX register
    process (clk)
    begin
        if (rising_edge(clk)) then
            if reset = '0' or flush_pipe_i = '1' then
                execute_valid_s         <= '0';
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
                execute_lsq_idx_s       <= (others => '0'); 
                execute_instruction     <= (others => '0'); 
                 
            elsif stall_ex_i = '0' then
                execute_valid_s         <= issue_valid_s;
                execute_rs1_data_s      <= issue_rs1_data_s;
                execute_rs2_data_s      <= issue_rs2_data_s;
                execute_rs1_addr_s      <= issue_rs1_addr_s;
                execute_rs2_addr_s      <= issue_rs2_addr_s;
                execute_rd_addr_s       <= issue_rd_addr_s;
                execute_instr_type_s    <= issue_instr_type_s;
                execute_instr_subtype_s <= issue_instr_subtype_s;
                execute_rob_idx_s       <= issue_rob_idx_s; 
                execute_lsq_idx_s       <= issue_lsq_idx_s; 
                execute_imm_s           <= issue_imm_s;     
                execute_pc_reg_s        <= issue_pc_reg_s;     
                execute_instruction     <= issue_instruction;
            end if;
        end if;
    end process;
    
    -- Jump target address
    generate_rob_jump_addr: 
    process(execute_valid_s, execute_instr_type_s, execute_rs1_data_s, execute_pc_reg_s, execute_imm_s, execute_rob_idx_s)
    begin
        rob_branch_addr_valid_s <= '0';
        rob_branch_addr_s       <= (others => '0');
        rob_branch_idx_s        <= execute_rob_idx_s;
        
        if (execute_valid_s = '0') then
            null;
        elsif (execute_instr_type_s = BRANCH) then
            rob_branch_addr_valid_s <= '1';
            rob_branch_addr_s <= std_logic_vector(unsigned(execute_pc_reg_s) + to_unsigned(4, DATA_WIDTH));
        end if;
    end process;
    
    -- JALR redirect from EX, registered
    jalr_redirect:
    process (clk)
    begin
        if rising_edge(clk) then
            if reset = '0' or flush_pipe_i = '1' then
                jalr_wait_s          <= '0';
                jalr_redirect_s      <= '0';
                jalr_redirect_addr_s <= (others => '0');
            else
                jalr_redirect_s <= '0';
                if (decode_is_jalr_s = '1' and stall_id_i = '0') then
                    jalr_wait_s <= '1';
                elsif (jalr_redirect_s = '1') then
                    jalr_wait_s <= '0';
                end if;
                
                if (jalr_wait_s = '1' and jalr_redirect_s = '0' and execute_valid_s = '1' and execute_instr_type_s = JALR) then
                    jalr_redirect_s      <= '1';
                    jalr_redirect_addr_s <= std_logic_vector(unsigned(execute_rs1_data_s) + unsigned(execute_imm_s)) and x"FFFFFFFE";
                end if;
            end if;
        end if;
    end process;
    
    -- LSQ allocation at rename
    rename_is_mem_s     <= '1' when (rename_instruction(6 downto 0) = OPCODE_LOAD or rename_instruction(6 downto 0) = OPCODE_STORE) else '0';
    rename_is_store_s   <= '1' when rename_instruction(6 downto 0) = OPCODE_STORE else '0';
    lsq_alloc_valid_s   <= rename_instr_valid_s and rename_is_mem_s and (not stall_rn_i) and (not flush_pipe_i);
    
    lsq_full_o          <= lsq_full_s and rename_is_mem_s and rename_instr_valid_s;
    
    ex_busy_o <= ex_busy_s;
    
    -- Unit valids do not depend on the stall (it comes from the CDB grant)
    ex_valid_s <= execute_valid_s and (not flush_pipe_i);

    -- IS/EX steering to units
    u_ex_router : entity work.ex_router
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            ROB_ADDR_BITS   => ROB_ADDR_BITS
        )
        port map (
            valid_i             => ex_valid_s,
            instr_type_i        => execute_instr_type_s,
            rs1_data_i          => execute_rs1_data_s,
            rs2_data_i          => execute_rs2_data_s,
            imm_i               => execute_imm_s,
            pc_i                => execute_pc_reg_s,
            rob_idx_i           => execute_rob_idx_s,
            alu_grant_i         => alu_cdb_grant_s,
            ex_busy_o           => ex_busy_s,
            alu_valid_o         => alu_valid_s,
            alu_op_a_o          => alu_op_a_s,
            alu_op_b_o          => alu_op_b_s,
            branch_valid_o      => branch_valid_s,
            lsq_fill_valid_o    => lsq_fill_valid_s,
            lsq_fill_addr_o     => lsq_fill_addr_s,
            store_done_valid_o  => store_done_valid_s,
            store_done_idx_o    => store_done_idx_s
        );

    u_alu_unit : entity work.alu_unit
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            PHYS_ADDR_BITS  => PHYS_ADDR_BITS,
            ROB_ADDR_BITS   => ROB_ADDR_BITS
        )
        port map (
            valid_i         => alu_valid_s,
            op_a_i          => alu_op_a_s,
            op_b_i          => alu_op_b_s,
            subtype_i       => execute_instr_subtype_s,
            rd_addr_i       => execute_rd_addr_s,
            rob_idx_i       => execute_rob_idx_s,
            cdb_req_o       => alu_cdb_req_s,
            cdb_grant_i     => alu_cdb_grant_s,
            cdb_data_o      => alu_cdb_data_s,
            cdb_rd_addr_o   => alu_cdb_rd_addr_s,
            cdb_rob_idx_o   => alu_cdb_rob_idx_s
        );

    u_branch_unit : entity work.branch_unit
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            ROB_ADDR_BITS   => ROB_ADDR_BITS
        )
        port map (
            clk             => clk,
            reset           => reset,
            flush_i         => flush_pipe_i,
            valid_i         => branch_valid_s,
            rs1_data_i      => execute_rs1_data_s,
            rs2_data_i      => execute_rs2_data_s,
            subtype_i       => execute_instr_subtype_s,
            rob_idx_i       => execute_rob_idx_s,
            taken_valid_o   => branch_taken_valid_s,
            taken_o         => branch_taken_s,
            taken_idx_o     => branch_taken_idx_s
        );

    -- LSQ between rename, AGU, CDB, memory
    u_lsq : entity work.LSQ
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            MEM_BYTES       => MEM_BYTES,
            PHYS_ADDR_BITS  => PHYS_ADDR_BITS,
            ROB_ADDR_BITS   => ROB_ADDR_BITS,
            LSQ_DEPTH       => LSQ_DEPTH,
            LSQ_BITS        => LSQ_BITS
        )
        port map (
            clk                 => clk,
            reset               => reset,
            alloc_valid_i       => lsq_alloc_valid_s,
            alloc_is_store_i    => rename_is_store_s,
            lsq_tail_idx_o      => lsq_tail_idx_s,
            lsq_full_o          => lsq_full_s,
            fill_valid_i        => lsq_fill_valid_s,
            fill_idx_i          => execute_lsq_idx_s,
            fill_addr_i         => lsq_fill_addr_s,
            fill_wdata_i        => execute_rs2_data_s,
            fill_subtype_i      => execute_instr_subtype_s,
            fill_rd_addr_i      => execute_rd_addr_s,
            fill_rob_idx_i      => execute_rob_idx_s,
            commit_store_i      => rob_commit_store_s,
            flush_pipe_i        => flush_pipe_i,
            cdb_req_o           => lsq_cdb_req_s,
            cdb_grant_i         => lsq_cdb_grant_s,
            cdb_data_o          => lsq_cdb_data_s,
            cdb_rd_addr_o       => lsq_cdb_rd_addr_s,
            cdb_rob_idx_o       => lsq_cdb_rob_idx_s,
            data_mem_req_o      => data_mem_req_o,
            data_mem_ready_i    => data_mem_ready_i,
            data_mem_we_o       => data_mem_we_o,
            data_mem_addr_o     => data_mem_addr_o,
            data_mem_wdata_o    => data_mem_wdata_o,
            data_mem_be_o       => data_mem_be_o,
            data_mem_rsp_i      => data_mem_rsp_i,
            data_mem_rdata_i    => data_mem_rdata_i
        );

    -- CDB from ALU and LSQ
    u_cdb : entity work.cdb
        generic map (
            DATA_WIDTH      => DATA_WIDTH,
            PHYS_ADDR_BITS  => PHYS_ADDR_BITS,
            ROB_ADDR_BITS   => ROB_ADDR_BITS
        )
        port map (
            clk             => clk,
            reset           => reset,
            flush_i         => flush_pipe_i,
            alu_req_i       => alu_cdb_req_s,
            alu_grant_o     => alu_cdb_grant_s,
            alu_data_i      => alu_cdb_data_s,
            alu_rd_addr_i   => alu_cdb_rd_addr_s,
            alu_rob_idx_i   => alu_cdb_rob_idx_s,
            lsq_req_i       => lsq_cdb_req_s,
            lsq_grant_o     => lsq_cdb_grant_s,
            lsq_data_i      => lsq_cdb_data_s,
            lsq_rd_addr_i   => lsq_cdb_rd_addr_s,
            lsq_rob_idx_i   => lsq_cdb_rob_idx_s,
            cdb_valid_o     => cdb_valid_s,
            cdb_data_o      => cdb_data_s,
            cdb_addr_o      => cdb_rd_addr_s,
            cdb_rob_idx_o   => wb_rob_idx_s
        );

    -- Commit stage, ROB
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
            
            -- Rename ports
            rename_instr_valid_i    => rename_instr_valid_s and (not stall_rn_i) and (not flush_pipe_i),   
            
            rename_rd_arch_i        => rob_rename_rd_arch_s,   
            rename_rd_phys_i        => rob_rename_rd_phys_s,   
            rename_prev_phys_i      => rob_rename_prev_phys_s,
            rename_rd_instr_i       => rob_rename_rd_instr_s, 
            rob_tail_idx_o          => rob_rename_tail_idx_s,    
            rename_is_store_i       => rename_is_store_s,
            rename_instruction_i    => rename_instruction,
            
            -- Write-back and branch ports
            wb_valid_i              => cdb_valid_s,
            wb_rob_idx_i            => wb_rob_idx_s,
            
            store_done_valid_i      => store_done_valid_s,
            store_done_idx_i        => store_done_idx_s,
            
            branch_addr_i           => rob_branch_addr_s,
            branch_addr_valid_i     => rob_branch_addr_valid_s,
            branch_addr_tag_i       => rob_branch_idx_s,
            
            branch_taken_i          => branch_taken_s,
            branch_taken_valid_i    => branch_taken_valid_s,
            branch_taken_idx_i      => branch_taken_idx_s,
            
            -- Commit ports
            commit_valid_o          => rob_commit_valid_s,
            commit_rd_arch_o        => rob_commit_rd_arch_s,
            commit_rd_phys_o        => rob_commit_rd_phys_s,
            commit_prev_phys_o      => rob_commit_prev_phys_s,
            commit_rd_instr_o       => rob_commit_rd_instr_s,
            commit_store_o          => rob_commit_store_s,
            
            commit_jump_addr_o      => pc_revert_addr_s,
            misspredict_o           => misspredict_o,
            
            rob_full_o              => rob_full_s,
            rob_empty_o             => rob_empty_s
        );
    
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;
use work.alu_ops_pkg.all;

entity execute_module is
    generic (
        DATA_WIDTH      : natural := 32;
        PHYS_ADDR_BITS  : natural := 5;
        RS_DEPTH        : natural := 8;
        RS_BITS         : natural := 2;
        MEM_BYTES       : natural := 4;
        ROB_ADDR_BITS   : natural := 6 -- log2(64)
    );
    port (
        clk                     : in  std_logic;
        reset                   : in  std_logic;
        
        execute_valid_i         : in  std_logic;
        
        execute_rs1_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        execute_rs2_addr_i      : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        
        execute_rs1_data_i      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        execute_rs2_data_i      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        execute_rd_addr_i       : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        
        execute_instr_type_i    : in  std_logic_vector(3 downto 0);
        execute_instr_subtype_i : in  std_logic_vector(4 downto 0);
        
        execute_rs1_ready_i     : in  std_logic;
        execute_rs2_ready_i     : in  std_logic;
        
        execute_imm_i           : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        execute_pc_reg_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        execute_rob_idx_i       : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        
        rs_full_o               : out std_logic;
        branch_taken_o          : out std_logic;
        branch_imm_o            : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        -- CDB Output
        cdb_valid_o             : out std_logic;
        cdb_data_o              : out std_logic_vector(DATA_WIDTH-1 downto 0);
        cdb_addr_o              : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        wb_rob_idx_o            : out std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        
        -- Output Data Memory Interface (read/write)
        data_mem_addr_o         : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Address for data access
        data_mem_wdata_o        : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data to be written to memory
        data_mem_rdata_i        : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data read from memory
        data_mem_be_o           : out std_logic_vector(MEM_BYTES-1 downto 0)   -- Byte enable (store strobe)    
      
    );
end entity;

architecture Behavioral of execute_module is
    
    -- ALU RSs
    type rs_alu_ffs_entry_t is record
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
    end record;
    
    type rs_alu_ffs_array_t is array (0 to RS_DEPTH-1) of rs_alu_ffs_entry_t;
    
    -- LSU RSs
    type rs_lsu_ffs_entry_t is record
        valid           : std_logic;
        rs1_ready       : std_logic;
        rs2_ready       : std_logic;
        is_load         : std_logic;
        rs1_data        : std_logic_vector(DATA_WIDTH-1 downto 0);
        rs2_data        : std_logic_vector(DATA_WIDTH-1 downto 0);
        imm             : std_logic_vector(DATA_WIDTH-1 downto 0);
        rs1_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rs2_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        instr_subtype   : std_logic_vector(4 downto 0);
        rd_addr         : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rob_idx         : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    end record;
    
    type rs_lsu_ffs_array_t is array (0 to RS_DEPTH-1) of rs_lsu_ffs_entry_t;
    
    -- BRANCH RSs
    type rs_branch_ffs_entry_t is record
        valid           : std_logic;
        rs1_ready       : std_logic;
        rs2_ready       : std_logic;
        instr_subtype   : std_logic_vector(4 downto 0);
        rs1_data        : std_logic_vector(DATA_WIDTH-1 downto 0);
        rs2_data        : std_logic_vector(DATA_WIDTH-1 downto 0);
        rs1_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rs2_addr        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rob_idx         : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    end record;
    
    type rs_branch_ffs_array_t is array (0 to RS_DEPTH-1) of rs_branch_ffs_entry_t;
    
    signal rs_alu_ffs_arr_s     : rs_alu_ffs_array_t;
    
    signal rs_lsu_ffs_arr_s     : rs_lsu_ffs_array_t;
    
    signal rs_branch_ffs_arr_s  : rs_branch_ffs_array_t;
    
    -- Ready RS entry signals
    signal alu_rs_entry_ready_s         : std_logic;
    signal alu_rs_entry_addr_s          : std_logic_vector(RS_BITS-1 downto 0);
    signal lsu_rs_entry_ready_s         : std_logic;
    signal lsu_rs_entry_addr_s          : std_logic_vector(RS_BITS-1 downto 0);
    signal branch_rs_entry_ready_s      : std_logic;
    signal branch_rs_entry_addr_s       : std_logic_vector(RS_BITS-1 downto 0);
    
    -- Execute RS signals
    signal rs_alu_execute_valid_s       : std_logic;
    signal rs_alu_execute_addr_s        : std_logic_vector(RS_BITS-1 downto 0);
    signal rs_lsu_execute_valid_s       : std_logic;
    signal rs_lsu_execute_addr_s        : std_logic_vector(RS_BITS-1 downto 0);
    signal rs_branch_execute_valid_s    : std_logic;
    signal rs_branch_execute_addr_s     : std_logic_vector(RS_BITS-1 downto 0);
    
    -- RS full signals
    signal alu_rs_full_s                : std_logic;
    signal lsu_rs_full_s                : std_logic;
    signal branch_rs_full_s             : std_logic;
            
    -- CDB signals
    signal alu_cdb_data_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_cdb_rd_addr_s            : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal alu_cdb_req_s                : std_logic;
    signal alu_rob_idx_s                : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    
    signal alu_pre_cdb_rd_addr_s        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    signal lsu_cdb_data_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal lsu_cdb_rd_addr_s            : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal lsu_cdb_req_s                : std_logic;
    signal lsu_rob_idx_s                : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    
    signal cdb_data_s                   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal cdb_addr_s                   : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal cdb_valid_s                  : std_logic;
    
    signal last_grant_s                 : std_logic;
    signal grant_alu_s                  : std_logic;
    signal grant_lsu_s                  : std_logic;
    
    signal is_load_s                    : std_logic;
    
    signal lsu_pre_cdb_rd_addr_s        : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    signal rs1_s                        : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rs2_s                        : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- ALU signals
    -- rs_alu_ram_entry_t signal declarations
    signal rs1_data_s                   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rs2_data_s                   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rs_imm_s                     : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal rs1_addr_s                   : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rs2_addr_s                   : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rd_addr_s                    : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal instr_subtype_s              : std_logic_vector(4 downto 0);
    
    -- rs_alu_ffs_entry_t signal declarations
    signal rs_valid_s                   : std_logic;
    signal rs1_ready_s                  : std_logic;
    signal rs2_ready_s                  : std_logic;

    signal alu_a_s                      : std_logic_vector(31 downto 0);
    signal alu_b_s                      : std_logic_vector(31 downto 0);
    signal alu_op_s                     : std_logic_vector(4 downto 0);
    signal alu_result_s                 : std_logic_vector(31 downto 0);
    signal alu_zero_s                   : std_logic;
    signal alu_overflow_s               : std_logic;
    
    -- LSU signals
    signal lsu_addr_i_s                 : std_logic_vector(31 downto 0);
    signal lsu_wdata_i_s                : std_logic_vector(31 downto 0);
    signal lsu_rdata_o_s                : std_logic_vector(31 downto 0);
    
    signal lsu_addr_o_s                 : std_logic_vector(31 downto 0);
    signal lsu_wdata_o_s                : std_logic_vector(31 downto 0);
    signal lsu_rdata_i_s                : std_logic_vector(31 downto 0);
    signal lsu_be_o_s                   : std_logic_vector(3 downto 0);
    
    signal lsu_subtype_s                : std_logic_vector(4 downto 0);
    
    -- Branch Unit signals
    signal branch_a_s                   : std_logic_vector(31 downto 0);
    signal branch_b_s                   : std_logic_vector(31 downto 0);
    signal branch_subtype_s             : std_logic_vector(4 downto 0);
    signal branch_taken_s               : std_logic;
    signal branch_rob_idx_s             : std_logic_vector(ROB_ADDR_BITS-1 downto 0);
    
begin
    
    operands_mux:
    process (execute_instr_type_i, execute_rs1_data_i, execute_rs2_data_i, execute_imm_i)
    begin
        case execute_instr_type_i is
            when R_TYPE =>      
                rs1_s   <= execute_rs1_data_i;
                rs2_s   <= execute_rs2_data_i;
                
            when I_TYPE =>         
                rs1_s   <= execute_rs1_data_i;
                rs2_s   <= execute_imm_i;
                
            -- possibly one separate port to cdb for immediate publish much earlier
            when LUI =>   
                rs1_s   <= (others =>'0');
                rs2_s   <= execute_imm_i;
                
            -- possibly one separate port to cdb for immediate publish much earlier
            when AUIPC =>   
                rs1_s   <= execute_pc_reg_i;
                rs2_s   <= execute_imm_i;
                
            -- possibly one separate port to cdb for immediate publish much earlier
            when JAL | JALR =>   
                rs1_s         <= execute_pc_reg_i;
                rs2_s         <= std_logic_vector(to_unsigned(4, DATA_WIDTH));
                
            when others =>
                rs1_s   <= (others =>'0');   
                rs2_s   <= (others =>'0');   
        end case;
    end process;
                
    -- FFs WRITE
    RS_ffs:process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                for i in 0 to RS_DEPTH-1 loop
                    rs_alu_ffs_arr_s(i).valid         <= '0';
                    rs_alu_ffs_arr_s(i).rs1_ready     <= '0';
                    rs_alu_ffs_arr_s(i).rs2_ready     <= '0';
                    rs_alu_ffs_arr_s(i).rs1_data      <= (others => '0');
                    rs_alu_ffs_arr_s(i).rs2_data      <= (others => '0');
        
                    rs_lsu_ffs_arr_s(i).valid         <= '0';
                    rs_lsu_ffs_arr_s(i).rs1_ready     <= '0';
                    rs_lsu_ffs_arr_s(i).rs2_ready     <= '0';
                    rs_lsu_ffs_arr_s(i).rs1_data      <= (others => '0');
                    rs_lsu_ffs_arr_s(i).rs2_data      <= (others => '0');
        
                    rs_branch_ffs_arr_s(i).valid       <= '0';
                    rs_branch_ffs_arr_s(i).rs1_ready   <= '0';
                    rs_branch_ffs_arr_s(i).rs2_ready   <= '0';
                    rs_branch_ffs_arr_s(i).rs1_data    <= (others => '0');
                    rs_branch_ffs_arr_s(i).rs2_data    <= (others => '0');
                end loop;

                branch_rs_full_s    <= '0';
                alu_rs_full_s       <= '0';
                lsu_rs_full_s       <= '0';
                
            else
                -- Default values before the instruction is decoded
                is_load_s           <= '0'; -- should be in flipflops
                alu_rs_full_s    <= '0';
                lsu_rs_full_s    <= '0';
                branch_rs_full_s <= '0';
                
                -- FFs WRITE 
                -- Reservation stations FFs
                case execute_instr_type_i is
                    when R_TYPE | I_TYPE | LUI | AUIPC | JAL | JALR =>  -- ALU
                        alu_rs_full_s <= '1';
                        if(alu_rs_entry_ready_s = '1') then
                            alu_rs_full_s <= '0';
                            if(execute_valid_i = '1') then        
                                rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).valid           <= '1';
                                rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs1_addr        <= execute_rs1_addr_i;
                                rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs2_addr        <= execute_rs2_addr_i;
                                rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rd_addr         <= execute_rd_addr_i;
                                rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).instr_subtype   <= execute_instr_subtype_i;
                                rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rob_idx         <= execute_rob_idx_i;
                                  
                                -- Take the cdb data if published else read from IS/EX reg
                                if(execute_rs1_addr_i = cdb_addr_s and cdb_valid_s = '1' and rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs1_ready = '0') then
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs1_ready   <= '1';
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs1_data    <= cdb_data_s;
                                else
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs1_ready   <= execute_rs1_ready_i;
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs1_data    <= rs1_s;
                                end if;
                                
                                -- Take the cdb data if published else read from IS/EX reg
                                if(execute_rs2_addr_i = cdb_addr_s and cdb_valid_s = '1' and rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs2_ready = '0') then
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs2_ready    <= '1';
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs2_data     <= cdb_data_s;
                                else
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs2_ready    <= execute_rs2_ready_i;
                                    rs_alu_ffs_arr_s(to_integer(unsigned(alu_rs_entry_addr_s))).rs2_data     <= rs2_s;
                                end if;
                            end if;
                        end if;
                                            
                    when LOAD | STORE =>  -- LSU
                        lsu_rs_full_s <= '1';
                        if(lsu_rs_entry_ready_s = '1') then
                            lsu_rs_full_s <= '0';
                            if(execute_valid_i = '1') then
                                -- if cdb 
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).valid         <= '1';
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).imm           <= execute_imm_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs1_data      <= execute_rs1_data_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs2_data      <= execute_rs2_data_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs1_ready     <= execute_rs1_ready_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs2_ready     <= execute_rs2_ready_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs1_addr      <= execute_rs1_addr_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs2_addr      <= execute_rs2_addr_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).instr_subtype <= execute_instr_subtype_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rd_addr       <= execute_rd_addr_i;
                                rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rob_idx       <= execute_rob_idx_i;
                                
                                if(execute_instr_type_i = LOAD) then
                                    rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).is_load       <= '1';
                                else
                                    rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).is_load       <= '0';
                                end if;
                                
                            end if;
                        end if;
                    
                    when BRANCH =>  -- BRANCH
                        branch_rs_full_s <= '1';
                        if(branch_rs_entry_ready_s = '1') then
                            branch_rs_full_s <= '0';
                            if(execute_valid_i = '1') then
                                -- if cdb 
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).valid         <= '1';
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs1_data      <= execute_rs1_data_i;
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs2_data      <= execute_rs2_data_i;
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs1_ready     <= execute_rs1_ready_i;
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs2_ready     <= execute_rs2_ready_i;
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs1_addr      <= execute_rs1_addr_i;
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs2_addr      <= execute_rs2_addr_i;
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).instr_subtype <= execute_instr_subtype_i;
                                rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rob_idx             <= execute_rob_idx_i;
                            end if;
                        end if;
                    
                    when others =>
                        null;
                end case;
                
                -- Reset the entry when issuing from the RS
                -- Key assumption: grant_x_s means x buffer is ready (x data forwarded to cdb)
                -- ALU
                if(alu_cdb_req_s = '0' or (alu_cdb_req_s = '1' and grant_alu_s = '1')) then
                    if(rs_alu_execute_valid_s = '1') then
                        rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).valid     <= '0';
                        rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).rs1_ready <= '0';
                        rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).rs2_ready <= '0';
                    end if;
                end if;      
                
                -- LSU
                if(lsu_cdb_req_s = '0' or (lsu_cdb_req_s = '1' and grant_lsu_s = '1')) then
                    if(rs_lsu_execute_valid_s = '1') then  
                        rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).valid      <= '0';
                        rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs1_ready  <= '0';
                        rs_lsu_ffs_arr_s(to_integer(unsigned(lsu_rs_entry_addr_s))).rs2_ready  <= '0';
                    end if;     
                end if; 
                
                -- Branch
                -- if ready ???
                if(rs_branch_execute_valid_s = '1') then
                    rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).valid     <= '0';
                    rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs1_ready <= '0';
                    rs_branch_ffs_arr_s(to_integer(unsigned(branch_rs_entry_addr_s))).rs2_ready <= '0';
                end if; 
                
                -- Update RS entries with new CDB data
                if (cdb_valid_s = '1') then
                    for i in 0 to RS_DEPTH-1 loop
                        -- rsx_ready = 0 nesseccary ?
                        -- ALU
                        if (rs_alu_ffs_arr_s(i).valid = '1') then
                            if (rs_alu_ffs_arr_s(i).rs1_ready = '0' and rs_alu_ffs_arr_s(i).rs1_addr = cdb_addr_s) then
                                rs_alu_ffs_arr_s(i).rs1_ready <= '1';
                                rs_alu_ffs_arr_s(i).rs1_data  <= cdb_data_s;
                            end if;
            
                            if (rs_alu_ffs_arr_s(i).rs2_ready = '0' and rs_alu_ffs_arr_s(i).rs2_addr = cdb_addr_s) then
                                rs_alu_ffs_arr_s(i).rs2_ready <= '1';
                                rs_alu_ffs_arr_s(i).rs2_data  <= cdb_data_s;
                            end if;
                        end if;
                        
                        -- LSU
                        if (rs_lsu_ffs_arr_s(i).valid = '1') then
                            if (rs_lsu_ffs_arr_s(i).rs1_ready = '0' and rs_lsu_ffs_arr_s(i).rs1_addr = cdb_addr_s) then
                                rs_lsu_ffs_arr_s(i).rs1_ready <= '1';
                                rs_lsu_ffs_arr_s(i).rs1_data  <= cdb_data_s;
                            end if;
            
                            if (rs_lsu_ffs_arr_s(i).rs2_ready = '0' and rs_lsu_ffs_arr_s(i).rs2_addr = cdb_addr_s) then
                                rs_lsu_ffs_arr_s(i).rs2_ready <= '1';
                                rs_lsu_ffs_arr_s(i).rs2_data  <= cdb_data_s;
                            end if;
                        end if;
                        
                        -- Branch
                        if (rs_branch_ffs_arr_s(i).valid = '1') then
                            if (rs_branch_ffs_arr_s(i).rs1_ready = '0' and rs_branch_ffs_arr_s(i).rs1_addr = cdb_addr_s) then
                                rs_branch_ffs_arr_s(i).rs1_ready <= '1';
                                rs_branch_ffs_arr_s(i).rs1_data  <= cdb_data_s;
                            end if;
            
                            if (rs_branch_ffs_arr_s(i).rs2_ready = '0' and rs_branch_ffs_arr_s(i).rs2_addr = cdb_addr_s) then
                                rs_branch_ffs_arr_s(i).rs2_ready <= '1';
                                rs_branch_ffs_arr_s(i).rs2_data  <= cdb_data_s;
                            end if;
                        end if; 
                    end loop;
                end if;
            end if;
        end if;
    end process;

    -- FFs READ
    ffs_read:process(alu_cdb_req_s, grant_alu_s, rs_alu_execute_valid_s, rs_alu_ffs_arr_s, rs_alu_execute_addr_s, 
                        rs_lsu_execute_valid_s, rs_lsu_ffs_arr_s, rs_lsu_execute_addr_s, rs_branch_execute_valid_s, 
                            rs_branch_ffs_arr_s, rs_branch_execute_addr_s) is
    begin
        -- Issue from the reservation stations FFs
        -- Key assumption: grant_x_s means x buffer is ready (x data forwarded to cdb)
        
        -- ALU
        if(alu_cdb_req_s = '0' or (alu_cdb_req_s = '1' and grant_alu_s = '1')) then
            if(rs_alu_execute_valid_s = '1') then
                alu_a_s                 <= rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).rs1_data;
                alu_b_s                 <= rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).rs2_data;
                alu_op_s                <= rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).instr_subtype;
                alu_pre_cdb_rd_addr_s   <= rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).rd_addr;
                alu_rob_idx_s           <= rs_alu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).rob_idx;
            end if;
        end if;      
                       
        -- LSU 
        if(lsu_cdb_req_s = '0' or (lsu_cdb_req_s = '1' and grant_lsu_s = '1')) then
            if(rs_lsu_execute_valid_s = '1') then  
                is_load_s       <= '0';
                lsu_subtype_s   <= rs_lsu_ffs_arr_s(to_integer(unsigned(rs_lsu_execute_addr_s))).instr_subtype;
                lsu_addr_i_s   <= std_logic_vector(unsigned(rs_lsu_ffs_arr_s(to_integer(unsigned(rs_lsu_execute_addr_s))).rs1_data) + unsigned(rs_lsu_ffs_arr_s(to_integer(unsigned(rs_lsu_execute_addr_s))).imm));
                lsu_wdata_i_s  <= rs_lsu_ffs_arr_s(to_integer(unsigned(rs_lsu_execute_addr_s))).rs2_data; 
                lsu_rob_idx_s  <= rs_lsu_ffs_arr_s(to_integer(unsigned(rs_lsu_execute_addr_s))).rob_idx;
                
                if (rs_lsu_ffs_arr_s(to_integer(unsigned(rs_alu_execute_addr_s))).is_load = '1') then   -- correct ?
                    lsu_pre_cdb_rd_addr_s <= rs_lsu_ffs_arr_s(to_integer(unsigned(rs_lsu_execute_addr_s))).rd_addr; 
                    is_load_s             <= '1';
                end if;
            end if;    
        end if;     
        
        -- Branch
        -- if(ready)
        if(rs_branch_execute_valid_s = '1') then
            branch_a_s       <= rs_branch_ffs_arr_s(to_integer(unsigned(rs_branch_execute_addr_s))).rs1_data;  
            branch_b_s       <= rs_branch_ffs_arr_s(to_integer(unsigned(rs_branch_execute_addr_s))).rs2_data; 
            branch_subtype_s <= rs_branch_ffs_arr_s(to_integer(unsigned(rs_branch_execute_addr_s))).instr_subtype;
            branch_taken_o   <= branch_taken_s;
        end if;  
    end process;
    
    -- ALU unit
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
        
    -- -- ALU FIFO buffer
    -- alu_fifo_buff : entity work.free_list_fifo
    -- generic map (
    --     DEPTH       => RS_DEPTH/2,
    --     DATA_WIDTH  => PHYS_ADDR_BITS
    -- )
    -- port map (
    --     clk       => clk,
    --     reset     => reset,
    --     push      => push_s,
    --     push_data => rob_commit_prev_phys_i,
    --     pop       => pop_s,
    --     pop_data  => pop_data_s,
    --     empty     => empty_flag_s,
    --     full      => full_flag_s
    -- );
    -- 
    -- -- ALU buffer
    -- -- RS must stall correctly
    -- alu_buffer: process (clk) is
    -- begin
    --     if(rising_edge(clk)) then
    --         if(reset = '0') then
    --             alu_cdb_data_s      <= (others => '0');
    --             alu_cdb_rd_addr_s   <= (others => '0');
    --             alu_cdb_req_s       <= '0';
    --         else
    --             alu_cdb_data_s      <= alu_result_s;
    --             alu_cdb_rd_addr_s   <= alu_pre_cdb_rd_addr_s;
    --             alu_cdb_req_s       <= rs_alu_execute_valid_s;
    --         end if;
    --     end if;
    -- end process;
    
    alu_cdb_data_s      <= alu_result_s;
    alu_cdb_rd_addr_s   <= alu_pre_cdb_rd_addr_s;
    alu_cdb_req_s       <= rs_alu_execute_valid_s;
    
    -- CDB
    -- Arbiter logic
    arbiter: process(alu_cdb_req_s, lsu_cdb_req_s, last_grant_s)
    begin
        grant_alu_s  <= '0';
        grant_lsu_s  <= '0';
        
        case last_grant_s is
            when '0' =>  -- ALU last grant
                if lsu_cdb_req_s = '1' then
                    grant_lsu_s <= '1';
                elsif alu_cdb_req_s = '1' then
                    grant_alu_s <= '1';
                end if;
            when others => -- LSU last grant
                if alu_cdb_req_s = '1' then
                    grant_alu_s <= '1';
                elsif lsu_cdb_req_s = '1' then
                    grant_lsu_s <= '1';
                end if;
        end case;
    end process;
    
    -- CDB reg
    CDB_reg: process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                cdb_data_s      <= (others => '0');
                cdb_addr_s      <= (others => '0');
                cdb_valid_s     <= '0';
                last_grant_s    <= '0';
            else
                if grant_alu_s = '1' then
                    cdb_data_s      <= alu_cdb_data_s;
                    cdb_addr_s      <= alu_cdb_rd_addr_s;
                    cdb_valid_s     <= '1';
                    last_grant_s    <= '0'; 
                    wb_rob_idx_o    <= alu_rob_idx_s;
                elsif grant_lsu_s = '1' then
                    cdb_data_s      <= lsu_cdb_data_s;
                    cdb_addr_s      <= lsu_cdb_rd_addr_s;
                    cdb_valid_s     <= '1';
                    last_grant_s    <= '1'; 
                    wb_rob_idx_o    <= lsu_rob_idx_s;
                else
                    cdb_data_s      <= (others => '0');
                    cdb_addr_s      <= (others => '0');
                    cdb_valid_s     <= '0';
                end if;
            end if;
        end if;
    end process;
    
    cdb_data_o    <= cdb_data_s;
    cdb_addr_o    <= cdb_addr_s;
    cdb_valid_o   <= cdb_valid_s;
    
    -- -- LSU
    -- u_lsu: entity work.LSU
    --     generic map (
    --         DATA_WIDTH => DATA_WIDTH,
    --         MEM_BYTES  => MEM_BYTES
    --     )
    --     port map (
    --         data_mem_addr_i   => lsu_addr_i_s,
    --         data_mem_wdata_i  => lsu_wdata_i_s,
    --         data_mem_rdata_o  => lsu_rdata_o_s,
    --         
    --         lsu_subtype_i     => lsu_subtype_s,
    --         
    --         data_mem_addr_o   => data_mem_addr_o,
    --         data_mem_wdata_o  => data_mem_wdata_o,
    --         data_mem_rdata_i  => data_mem_rdata_i,
    --         data_mem_be_o     => data_mem_be_o   
    --     );
    --     
    -- -- LSU buffer
    -- process (clk) is
    -- begin
    --     if(rising_edge(clk)) then
    --         if(reset = '0') then
    --             lsu_cdb_data_s      <= (others => '0');
    --             lsu_cdb_rd_addr_s   <= (others => '0');
    --             lsu_cdb_req_s     <= '0';
    --         else
    --             if(is_load_s = '1') then -- if load pass the data to cdb
    --                 lsu_cdb_data_s      <= lsu_rdata_o_s;
    --                 lsu_cdb_rd_addr_s   <= lsu_pre_cdb_rd_addr_s;
    --                 lsu_cdb_req_s     <= rs_lsu_execute_valid_s;
    --             else    -- if store nothing to publish
    --                 lsu_cdb_data_s      <= (others => '0');
    --                 lsu_cdb_rd_addr_s   <= (others => '0');
    --                 lsu_cdb_req_s     <= '0';
    --             end if;
    --         end if;
    --     end if;
    -- end process;
    -- 
    -- -- BRANCH
    -- u_branch: entity work.branch_decision_unit
    --     generic map (
    --         DATA_WIDTH => DATA_WIDTH
    --     )
    --     port map (
    --         branch_condition_o => branch_taken_s,
    --         a_i                => branch_a_s,
    --         b_i                => branch_b_s,
    --         funct3_i           => branch_subtype_s
    --     );
    
    rs_full_o <= branch_rs_full_s or alu_rs_full_s or lsu_rs_full_s;

    -- Generic priority encoders for finding 
    -- 1) free slot for entry 
    -- 2) ready instruction with both operands to execute
    
    process(rs_alu_ffs_arr_s, rs_lsu_ffs_arr_s, rs_branch_ffs_arr_s, execute_instr_type_i) is
        variable idx   : integer   := -1;
        variable found : std_logic := '0';
    begin
    
    -- 1) free slot for entry 
        case execute_instr_type_i is
            when R_TYPE | I_TYPE | LUI | AUIPC | JAL | JALR =>  -- ALU
                found := '0';
                idx := 0;
                for i in 0 to RS_DEPTH-1 loop
                    if (rs_alu_ffs_arr_s(i).valid = '0' and found = '0') then
                        idx := i;
                        found := '1';
                    end if;
                end loop;
                
                if (found = '1') then
                    alu_rs_entry_ready_s <= '1';
                    alu_rs_entry_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));
                else
                    alu_rs_entry_ready_s <= '0';
                    alu_rs_entry_addr_s  <= (others => '0');
                end if;        
                
            when LOAD | STORE =>  -- LSU
                found := '0';
                idx := 0;
                for i in 0 to RS_DEPTH-1 loop
                    if (rs_lsu_ffs_arr_s(i).valid = '0' and found = '0') then
                        idx := i;
                        found := '1';
                    end if;
                end loop;
                
                if (found = '1') then
                    lsu_rs_entry_ready_s <= '1';
                    lsu_rs_entry_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));
                else
                    lsu_rs_entry_ready_s <= '0';
                    lsu_rs_entry_addr_s  <= (others => '0');
                end if;
            
            when BRANCH =>  -- Branch
                found := '0';
                idx := 0;
                for i in 0 to RS_DEPTH-1 loop
                    if (rs_branch_ffs_arr_s(i).valid = '0' and found = '0') then
                        idx := i;
                        found := '1';
                    end if;
                end loop;
                
                if (found = '1') then
                    branch_rs_entry_ready_s <= '1';
                    branch_rs_entry_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));
                else
                    branch_rs_entry_ready_s <= '0';
                    branch_rs_entry_addr_s <= (others => '0');
                end if;
                
            when others =>
                alu_rs_entry_ready_s    <= '0';            
                alu_rs_entry_addr_s     <= (others => '0');
                
                lsu_rs_entry_ready_s    <= '0';            
                lsu_rs_entry_addr_s     <= (others => '0');
                
                branch_rs_entry_ready_s <= '0';
                branch_rs_entry_addr_s  <= (others => '0');
        end case;
        
        -- 2) Find the ready instruction to execute (Read the RS ffs)
        -- ALU
        found := '0';
        idx := 0;
        for i in 0 to RS_DEPTH-1 loop
            if (rs_alu_ffs_arr_s(i).valid = '1' and rs_alu_ffs_arr_s(i).rs1_ready = '1' and rs_alu_ffs_arr_s(i).rs2_ready = '1' and found = '0') then
                idx := i;
                found := '1';
            end if;
        end loop;
        
        if (found = '1') then
            rs_alu_execute_valid_s <= '1';
            rs_alu_execute_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));
        else
            rs_alu_execute_valid_s <= '0';
            rs_alu_execute_addr_s <= (others => '0');
        end if;       
        
        -- LSU
        found := '0';
        idx := 0;
        for i in 0 to RS_DEPTH-1 loop
            if (rs_branch_ffs_arr_s(i).valid = '1' and rs_branch_ffs_arr_s(i).rs1_ready = '1' and rs_branch_ffs_arr_s(i).rs2_ready = '1' and found = '0') then
                idx := i;
                found := '1';
            end if;
        end loop;
        
        if (found = '1') then
            rs_lsu_execute_valid_s <= '1';
            rs_lsu_execute_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));
        else
            rs_lsu_execute_valid_s <= '0';
            rs_lsu_execute_addr_s <= (others => '0');
        end if;    
        
        -- Branch
        found := '0';
        idx := 0;
        for i in 0 to RS_DEPTH-1 loop
            if (rs_lsu_ffs_arr_s(i).valid = '1' and rs_lsu_ffs_arr_s(i).rs1_ready = '1' and rs_lsu_ffs_arr_s(i).rs2_ready = '1' and found = '0') then
                idx := i;
                found := '1';
            end if;
        end loop;
        
        if (found = '1') then
            rs_branch_execute_valid_s <= '1';
            rs_branch_execute_addr_s  <= std_logic_vector(to_unsigned(idx, RS_BITS));
        else
            rs_branch_execute_valid_s <= '0';
            rs_branch_execute_addr_s <= (others => '0');
        end if;       
    end process;
    
end Behavioral;

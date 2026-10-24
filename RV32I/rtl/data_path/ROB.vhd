library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ROB is
    generic (
        DATA_WIDTH      : natural := 32;
        ROB_DEPTH       : natural := 64;
        ROB_ADDR_BITS   : natural := 6;
        ARCH_ADDR_BITS  : natural := 5;
        NUM_PHYS_REGS   : natural := 64;
        PHYS_ADDR_BITS  : natural := 6
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;
        
        -- Rename stage
        rename_instr_valid_i: in  std_logic;
        rename_rd_arch_i    : in  std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
        rename_rd_phys_i    : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rename_prev_phys_i  : in  std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        rename_rd_instr_i   : in  std_logic;
        rename_instruction_i: in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rename_is_store_i   : in  std_logic;
        rob_tail_idx_o      : out std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- WB stage
        wb_valid_i          : in  std_logic;
        wb_rob_idx_i        : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- Store address/data in LSQ
        store_done_valid_i  : in  std_logic;
        store_done_idx_i    : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- Jump addr
        branch_addr_i       : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        branch_addr_valid_i : in  std_logic;
        branch_addr_tag_i   : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        
        branch_taken_i      : in  std_logic;
        branch_taken_valid_i: in  std_logic;
        branch_taken_idx_i  : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);
        
        misspredict_o       : out std_logic;
        
        -- Commit stage
        commit_valid_o      : out std_logic;
        commit_rd_arch_o    : out std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
        commit_rd_phys_o    : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        commit_prev_phys_o  : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
        commit_rd_instr_o   : out std_logic;
        commit_store_o      : out std_logic;

        commit_jump_addr_o  : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        rob_full_o          : out std_logic;
        rob_empty_o         : out std_logic
    );
end ROB;

architecture Behavioral of rob is
    
    type rob_entry is record
        valid       : std_logic;
        ready       : std_logic;
        rd_instr    : std_logic;  
        rd_arch     : std_logic_vector(4 downto 0);
        rd_phys     : std_logic_vector(5 downto 0);
        prev_phys   : std_logic_vector(5 downto 0);
        is_branch   : std_logic;
        is_store    : std_logic;
        branch_taken: std_logic;
        branch_addr : std_logic_vector(DATA_WIDTH-1 downto 0);
        instruction : std_logic_vector(DATA_WIDTH-1 downto 0);
    end record;
    type rob_array is array (0 to ROB_DEPTH - 1) of rob_entry;
    
    signal rob : rob_array;
    
    signal head, tail : integer range 0 to ROB_DEPTH - 1 := 0;
    
    signal rob_full_s       : std_logic;
    signal misspredict_s    : std_logic;
    
begin
    
    -- Rename stage
    process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                head    <= 0;
                tail    <= 0;
                rob     <=  (others => (valid => '0', ready => '0', rd_instr => '0', is_branch => '0', is_store => '0', branch_taken => '0', others => (others => '0')));

                commit_valid_o      <= '0';
                commit_store_o      <= '0';
                commit_jump_addr_o  <= (others => '0');
                commit_rd_arch_o    <= (others => '0');
                commit_rd_phys_o    <= (others => '0');
                commit_prev_phys_o  <= (others => '0');
                commit_rd_instr_o   <= '0';
                misspredict_s <= '0';
                
            else
                commit_valid_o      <= '0';
                commit_store_o      <= '0';

                -- Rename stage
                if rename_instr_valid_i = '1' and rob(tail).valid = '0' then
                    rob(tail).valid     <= '1';
                    if rename_instruction_i = x"00000013" then
                        rob(tail).ready     <= '1';
                    else
                        rob(tail).ready     <= '0';
                    end if;
                    rob(tail).rd_arch   <= rename_rd_arch_i;
                    rob(tail).rd_phys   <= rename_rd_phys_i;
                    rob(tail).prev_phys <= rename_prev_phys_i;
                    rob(tail).rd_instr  <= rename_rd_instr_i;
                    rob(tail).is_store  <= rename_is_store_i;
                    rob(tail).is_branch     <= '0';
                    rob(tail).branch_taken  <= '0';
                    rob(tail).instruction   <= rename_instruction_i;
                    tail                <= (tail + 1) mod ROB_DEPTH;
                end if;
                    
                -- Write-back stage    
                if (wb_valid_i = '1') then
                    rob(to_integer(unsigned(wb_rob_idx_i))).ready <= '1';
                end if;

                if (store_done_valid_i = '1') then
                    rob(to_integer(unsigned(store_done_idx_i))).ready <= '1';
                end if;
                
                -- Branch in    
                if (branch_addr_valid_i = '1') then
                    rob(to_integer(unsigned(branch_addr_tag_i))).branch_addr  <= branch_addr_i;
                    rob(to_integer(unsigned(branch_addr_tag_i))).is_branch    <= '1';
                end if;
                
                if(branch_taken_valid_i = '1') then
                    rob(to_integer(unsigned(branch_taken_idx_i))).branch_taken <= branch_taken_i;
                    rob(to_integer(unsigned(branch_taken_idx_i))).ready <= '1';
                end if;
                
                -- Commit stage
                if rob(head).valid = '1' and rob(head).ready = '1' then
                    commit_valid_o      <= '1';
                    commit_rd_instr_o   <= rob(head).rd_instr;
                    commit_rd_arch_o    <= rob(head).rd_arch;
                    commit_rd_phys_o    <= rob(head).rd_phys;
                    commit_prev_phys_o  <= rob(head).prev_phys;
                    commit_store_o      <= rob(head).is_store;

                    if(rob(head).is_branch = '1') then
                        if(rob(head).branch_taken = '0') then -- missprediction
                        
                            commit_jump_addr_o  <= rob(head).branch_addr;
                            misspredict_s       <= '1';
                            
                            -- Branch at head: everything younger is wrong path
                            -- (including the entry renamed in this same cycle)
                            for i in 0 to ROB_DEPTH-1 loop
                                rob(i).valid <= '0';
                            end loop;
                            
                            tail <= (head + 1) mod ROB_DEPTH;
                        end if;
                    end if;
                    
                    rob(head).valid <= '0';
                    head <= (head + 1) mod ROB_DEPTH;
                end if;
                if(misspredict_s = '1') then
                    misspredict_s <= '0';
                end if;
            end if;      
        end if;
    end process;
    
    misspredict_o   <= misspredict_s;
                        
    rob_tail_idx_o <= std_logic_vector(to_unsigned(tail, 6));

    -- pragma translate_off
    -- Deadlock check: valid ROB head not committing
    rob_deadlock_check: process(clk)
        constant STUCK_LIMIT : natural := 1000;    -- a load can wait behind a full LSQ of slow (AXI) stores
        variable head_prev   : integer range 0 to ROB_DEPTH - 1 := 0;
        variable stuck_cnt   : natural := 0;
    begin
        if rising_edge(clk) then
            if reset = '0' then
                stuck_cnt := 0;
            else
                if (rob(head).valid = '1' and head = head_prev) then
                    stuck_cnt := stuck_cnt + 1;
                else
                    stuck_cnt := 0;
                end if;
                head_prev := head;

                assert stuck_cnt < STUCK_LIMIT
                    report "ROB deadlock: head " & integer'image(head) & " not committed for " &
                           integer'image(STUCK_LIMIT) & " cycles"
                    severity failure;
            end if;
        end if;
    end process;
    -- pragma translate_on
    
    rob_empty_o <= '1' when head = tail and rob(head).valid = '0' else '0';
    rob_full_s  <= '1' when head = tail and rob(head).valid = '1' else '0';
    rob_full_o  <= rob_full_s;
    
end Behavioral;

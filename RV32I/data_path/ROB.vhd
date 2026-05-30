library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ROB is
    generic (
        ROB_DEPTH   : integer := 64;
        DATA_WIDTH  : integer := 32
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;
        
        -- Rename stage
        rob_write_en_i      : in  std_logic;
        rename_rd_arch_i    : in  std_logic_vector(4 downto 0);
        rename_rd_phys_i    : in  std_logic_vector(5 downto 0);
        rename_prev_phys_i  : in  std_logic_vector(5 downto 0);
        rename_rd_instr_i   : in  std_logic;
        rename_fifo_empty_i : in  std_logic;
        rob_tail_idx_o      : out std_logic_vector(5 downto 0);
        
        -- WB stage
        wb_valid_i          : in  std_logic;
        wb_rob_idx_i        : in  std_logic_vector(5 downto 0);
        
        -- Jump addr
        branch_addr_i       : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        branch_addr_valid_i : in  std_logic;
        branch_addr_tag_i   : in  std_logic_vector(5 downto 0);
        
        -- Commit stage
        commit_valid_o      : out std_logic;
        commit_rd_arch_o    : out std_logic_vector(4 downto 0);
        commit_rd_phys_o    : out std_logic_vector(5 downto 0);
        commit_prev_phys_o  : out std_logic_vector(5 downto 0);
        commit_rd_instr_o   : out std_logic;
        
        commit_jump_addr_o  : out std_logic_vector(DATA_WIDTH-1 downto 0);
        commit_pc_valid_o   : out std_logic;
        
        rob_full_o          : out std_logic;
        rob_empty_o         : out std_logic
    );
end ROB;

architecture Behavioral of rob is
    
    type rob_entry is record
        valid       : std_logic;
        ready       : std_logic;
        rd_instr    : std_logic;  
        is_branch   : std_logic;  
        rd_arch     : std_logic_vector(4 downto 0);
        rd_phys     : std_logic_vector(5 downto 0);
        prev_phys   : std_logic_vector(5 downto 0);
        branch_addr : std_logic_vector(DATA_WIDTH-1 downto 0);
    end record;
    type rob_array is array (0 to ROB_DEPTH - 1) of rob_entry;
    
    signal rob : rob_array;
    
    signal head, tail : integer range 0 to ROB_DEPTH - 1 := 0;
    
    signal rob_full_s : std_logic;
    
begin
    
    -- Rename stage
    process(clk)
    begin
        if (rising_edge(clk)) then
            if (reset = '0') then
                tail <= 0;
                for i in 0 to ROB_DEPTH - 1 loop
                    rob(i).valid        <= '0';
                    rob(i).ready        <= '0';
                    rob(i).rd_arch      <= (others => '0');
                    rob(i).rd_phys      <= (others => '0');
                    rob(i).prev_phys    <= (others => '0');
                    rob(i).branch_addr  <= (others => '0');
                    rob(i).rd_instr     <= '0';
                    rob(i).is_branch    <= '0';
                end loop;
            elsif (rob_write_en_i = '1' and rob_full_s = '0' and rename_fifo_empty_i = '0') then
                rob(tail).valid     <= '1';
                rob(tail).ready     <= '0';
                rob(tail).rd_arch   <= rename_rd_arch_i;
                rob(tail).rd_phys   <= rename_rd_phys_i;
                rob(tail).prev_phys <= rename_prev_phys_i;
                rob(tail).rd_instr  <= rename_rd_instr_i;
                tail <= (tail + 1) mod ROB_DEPTH;
            elsif (wb_valid_i = '1') then
                rob(to_integer(unsigned(wb_rob_idx_i))).ready <= '1';
            end if;
        end if;
    end process;
    
    rob_tail_idx_o <= std_logic_vector(to_unsigned(tail, 6));
    
    -- Branch addr
    process(clk)
    begin
        if rising_edge(clk) then
            if (branch_addr_valid_i = '1') then
                rob(to_integer(unsigned(branch_addr_tag_i))).branch_addr  <= branch_addr_i;
            end if;
        end if;
    end process;
    
    -- Commit stage
    process(clk)
    begin
        if rising_edge(clk) then
            if (reset = '0') then
                head           <= 0;
                commit_valid_o <= '0';
                commit_pc_valid_o   <= '0';
                commit_jump_addr_o  <= (others => '0');
                commit_rd_arch_o    <= (others => '0');
                commit_rd_phys_o    <= (others => '0');
                commit_prev_phys_o  <= (others => '0');
                commit_rd_instr_o   <= '0';
                
            elsif rob(head).valid = '1' and rob(head).ready = '1' then
                commit_pc_valid_o   <= '0';
                commit_valid_o      <= '1';
                commit_rd_arch_o    <= rob(head).rd_arch;
                commit_rd_phys_o    <= rob(head).rd_phys;
                commit_prev_phys_o  <= rob(head).prev_phys;
                commit_rd_instr_o   <= rob(head).rd_instr;
                if(rob(head).is_branch = '1') then
                    commit_jump_addr_o  <= rob(head).branch_addr;
                    commit_pc_valid_o   <= '1';
                else
                    commit_pc_valid_o   <= '0';
                end if;
                
                rob(head).valid <= '0';
                head <= (head + 1) mod ROB_DEPTH;
            else
                commit_valid_o <= '0';
            end if;
        end if;
    end process;
    
    rob_empty_o <= '1' when head = tail and rob(head).valid = '0' else '0';
    rob_full_s  <= '1' when head = tail and rob(head).valid = '1' else '0';
    rob_full_o  <= rob_full_s;
    
end Behavioral;

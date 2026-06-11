library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

entity renaming_module is
    generic(DATA_WIDTH      : positive  := 32;
            ARCH_ADDR_BITS  : natural   := 5;
            NUM_PHYS_REGS   : natural   := 64;
            PHYS_ADDR_BITS  : natural   := 6;
            ROB_DEPTH       : natural   := 64;
            ROB_ADDR_BITS   : natural   := 6 -- log2(64)
            );
    Port (  
            -- INPUTS
            clk   : in std_logic;
            reset : in std_logic;
            
            -- Architectural addr
            rs1_arch_addr_i     : in  STD_LOGIC_VECTOR (ARCH_ADDR_BITS-1 downto 0);
            rs2_arch_addr_i     : in  STD_LOGIC_VECTOR (ARCH_ADDR_BITS-1 downto 0);
            rd_arch_addr_i      : in  STD_LOGIC_VECTOR (ARCH_ADDR_BITS-1 downto 0);
            
            -- What registers are used in the instruction
            rename_rs1_used_i   : in  std_logic;
            rename_rs2_used_i   : in  std_logic;
            rd_we_i             : in  std_logic;
            rename_snapshot_i   : in  std_logic;
            flush_i             : in  std_logic;
            rename_instr_valid_i: in  std_logic;
            
            -- Operand outputs
            rs1_ready_o         : out std_logic;
            rs2_ready_o         : out std_logic;
            rs1_phys_addr_o     : out STD_LOGIC_VECTOR (PHYS_ADDR_BITS-1 downto 0);
            rs2_phys_addr_o     : out STD_LOGIC_VECTOR (PHYS_ADDR_BITS-1 downto 0);
            rd_phys_addr_o      : out STD_LOGIC_VECTOR (PHYS_ADDR_BITS-1 downto 0);
            
            -- WB ports for physical_ready_bits register
            cdb_valid_i         : in std_logic;
            cdb_rd_addr_i       : in std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
            
            -- ROB interface
            -- Renaming stage
            rob_rename_rd_instr_o     : out std_logic;
            rob_rename_rd_arch_o      : out std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
            rob_rename_rd_phys_o      : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
            rob_rename_prev_phys_o    : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
            
            -- Commit stage
            rob_commit_valid_i        : in std_logic;
            rob_commit_rd_instr_i     : in std_logic;
            rob_commit_rd_arch_i      : in std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
            rob_commit_rd_phys_i      : in std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
            rob_commit_prev_phys_i    : in std_logic_vector(PHYS_ADDR_BITS-1 downto 0);

            -- Status signals
            free_list_fifo_empty_o    : out std_logic;
            output_valid_o            : out std_logic
            );
            
end renaming_module;

architecture Behavioral of renaming_module is
    
    -- Physical addresses
    signal rs1_phys_addr_s              : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rs2_phys_addr_s              : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rd_phys_addr_s               : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    -- free_list fifo signals
    signal push_s, pop_s                : std_logic;
    signal full_flag_s                  : std_logic;
    signal empty_flag_s                 : std_logic;
    signal pop_data_s                   : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    -- ROB signals
    signal phys_ready_reg               : std_logic_vector(NUM_PHYS_REGS-1 downto 0);
    signal phys_ready_next              : std_logic_vector(NUM_PHYS_REGS-1 downto 0);
    signal phys_ready_snap_next              : std_logic_vector(NUM_PHYS_REGS-1 downto 0);
    
    signal phys_ready_reg_commit      : std_logic_vector(NUM_PHYS_REGS-1 downto 0);
    
    -- WE signal
    signal rename_table_we_s : std_logic;
    
    -- Renaming table
    type rename_table_array is array (0 to 31) of std_logic_vector(PHYS_ADDR_BITS-1 downto 0); -- 
    signal rename_table_s : rename_table_array;
    signal commit_rename_table_s : rename_table_array;
    
begin
    
    rs1_phys_addr_o <= rs1_phys_addr_s;
    rs2_phys_addr_o <= rs2_phys_addr_s;
    rd_phys_addr_o  <= rd_phys_addr_s;
    
    free_list_fifo_empty_o  <= empty_flag_s;

    -- COMMIT Renaming table
    -- Synchronous write
    write_commit_rename_table:
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '0' then
                commit_rename_table_s <= (others => (others => '0'));
            elsif rob_commit_valid_i = '1' then
                if to_integer(unsigned(rob_commit_rd_arch_i)) /= 0 then
                    commit_rename_table_s(to_integer(unsigned(rob_commit_rd_arch_i))) <= rob_commit_rd_phys_i;
                end if;
            end if;
        end if;
    end process;
    
    -- Renaming table
    -- Synchronous write
    write_rename_table_s:
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '0' then
                rename_table_s <= (others => (others => '0'));
            elsif flush_i = '1' then
                rename_table_s <= commit_rename_table_s;    
            elsif rename_table_we_s = '1' then
                if(to_integer(unsigned(rd_arch_addr_i)) /= 0) then
                    rename_table_s(to_integer(unsigned(rd_arch_addr_i))) <= rd_phys_addr_s;
                end if;
            end if;
        end if;
    end process;
    
    -- Asynchronous read
    reg_bank_read: 
    process (rs1_arch_addr_i, rs2_arch_addr_i, rename_table_s) is
    begin
    
       if(to_integer(unsigned(rs1_arch_addr_i)) = 0) then
          rs1_phys_addr_s <= std_logic_vector(to_unsigned(0, PHYS_ADDR_BITS));
       else
          rs1_phys_addr_s <= rename_table_s(to_integer(unsigned(rs1_arch_addr_i)));
       end if;

       if(to_integer(unsigned(rs2_arch_addr_i)) = 0) then
          rs2_phys_addr_s <= std_logic_vector(to_unsigned(0, PHYS_ADDR_BITS));
       else
          rs2_phys_addr_s <= rename_table_s(to_integer(unsigned(rs2_arch_addr_i)));
       end if;
    end process;
    
    -- Register to keep track of the ready bits of the physical registers
    physical_ready_bits_reg:
    process (clk) is
    begin
      if (rising_edge(clk)) then
         if (reset = '0')then
            phys_ready_reg <= std_logic_vector(to_unsigned(1, NUM_PHYS_REGS));
         elsif flush_i = '1' then
            phys_ready_reg <= phys_ready_reg_commit;
         else
            phys_ready_reg <= phys_ready_next;
         end if;
      end if;
    end process; 
            
    -- Priority logic to set/reset the ready bits
    -- 1) If the current register is being renamed then it's reset
    -- 2) If the current register is being written as ready from cdb, then it's set
    physical_ready_bits_next:
    process(rename_table_we_s, cdb_valid_i, rd_phys_addr_s, cdb_rd_addr_i, phys_ready_reg)
    begin
    
        phys_ready_next <= phys_ready_reg;

        for i in 0 to NUM_PHYS_REGS-1 loop
            if (rename_table_we_s = '1' and (to_integer(unsigned(rd_phys_addr_s)) = i)) then
                phys_ready_next(i) <= '0';
            elsif (cdb_valid_i = '1' and to_integer(unsigned(cdb_rd_addr_i)) = i) then
                phys_ready_next(i) <= '1';
            end if;
        end loop;
    end process;
    
    
    -- Commit version of the physical_ready_bits register
    commit_physical_ready_bits_reg:
    process (clk) is
    begin
        if (rising_edge(clk)) then
            if (reset = '0')then
                phys_ready_reg_commit <= (others => '0');
            elsif rename_snapshot_i = '1' then
                phys_ready_reg_commit <= phys_ready_next;
            else
                phys_ready_reg_commit <= phys_ready_snap_next;
            end if;
        end if;
    end process; 
    
    commit_physical_ready_bits_next:
    process(rob_commit_valid_i, rob_commit_rd_phys_i)
    begin
        phys_ready_snap_next <= phys_ready_reg_commit;
        for i in 0 to NUM_PHYS_REGS-1 loop
            if (rob_commit_valid_i = '1' and to_integer(unsigned(rob_commit_rd_phys_i)) = i) then
                phys_ready_snap_next(i) <= '1';
            end if;
        end loop;
    end process;
    
    
    -- Generate ready outputs for the operands
    -- If the operands are not used or phys_addr=0, they are considered ready to instigate execution
    -- cdb could publish just now on phys_ready_next and be missed so check is needed
    ready_gen:
    process(phys_ready_reg, rs1_phys_addr_s, rs2_phys_addr_s, rename_rs1_used_i, rename_rs2_used_i, cdb_rd_addr_i, cdb_valid_i) is 
    begin
        rs1_ready_o <= '0';
        rs2_ready_o <= '0';
        
            if(rename_rs1_used_i = '1') then
                if(cdb_valid_i = '1' and (rs1_phys_addr_s = cdb_rd_addr_i)) then
                    rs1_ready_o <= '1';
                else
                    rs1_ready_o <= phys_ready_reg(to_integer(unsigned(rs1_phys_addr_s)));
                end if;
            else
                rs1_ready_o <= '1';
            end if;
            
            if(rename_rs2_used_i = '1') then
                if (cdb_valid_i = '1' and (rs2_phys_addr_s = cdb_rd_addr_i)) then
                    rs2_ready_o <= '1';
                else
                    rs2_ready_o <= phys_ready_reg(to_integer(unsigned(rs2_phys_addr_s)));
                end if;
            else
                rs2_ready_o <= '1';
            end if;
    end process;
    
    
    -- Should this be in the control path ?
    -- Logic to handle free_list_fifo and rob entry in case it's (or not) an rd instr
    comb_logic: process (rd_we_i, pop_data_s, rd_arch_addr_i, rename_table_s, rename_instr_valid_i) is
    begin
        
        rd_phys_addr_s              <= (others => '0');
        pop_s                       <= '0';
        rename_table_we_s           <= '0';
        
        rob_rename_rd_instr_o       <= '0';
        rob_rename_rd_arch_o        <= (others => '0');
        rob_rename_rd_phys_o        <= (others => '0');
        rob_rename_prev_phys_o      <= (others => '0');
        
        if(rd_we_i = '1' and rename_instr_valid_i = '1') then
            -- Rd: New physical address from free_list_fifo
            rd_phys_addr_s          <= pop_data_s;
            pop_s                   <= '1';
            rename_table_we_s       <= '1';  -- Write it in the rename table
            
            -- Write the entry for the ROB
            rob_rename_rd_instr_o   <= '1'; 
            rob_rename_rd_arch_o    <= rd_arch_addr_i;
            rob_rename_rd_phys_o    <= pop_data_s;
            rob_rename_prev_phys_o  <= rename_table_s(to_integer(unsigned(rd_arch_addr_i))); -- Asynchronous read
        end if;
    end process;     
    
    -- Push rob_commit_prev_phys_i to free_list_fifo IFF 
    -- 1) rob output is valid
    -- 2) its's an rd instruction
    
    push_s <= '1' when (rob_commit_valid_i = '1' and rob_commit_rd_instr_i = '1') else
              '0';
    
    -- List of free physical registers (FIFO)
    u_free_list_fifo : entity work.free_list_fifo
    generic map (
        DEPTH       => NUM_PHYS_REGS,
        DATA_WIDTH  => PHYS_ADDR_BITS
    )
    port map (
        clk         => clk,
        reset       => reset,
        push        => push_s,
        push_data   => rob_commit_prev_phys_i,
        pop         => pop_s,
        snapshot_i  => rename_snapshot_i,
        flush_i     => flush_i,
        pop_data    => pop_data_s,
        empty       => empty_flag_s,
        full        => full_flag_s
    );
    
end Behavioral;

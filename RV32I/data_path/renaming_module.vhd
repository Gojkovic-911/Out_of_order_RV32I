library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.instr_types_pkg.all;

entity renaming_module is
   generic (DATA_WIDTH      : positive := 32;
            NUM_PHYS_REGS   : natural  := 64;
            TAG_WIDTH       : natural  := 6;
            ARCH_ADDR_BITS  : natural  := 5;
            PHYS_ADDR_BITS  : natural  := 5;
            ROB_DEPTH       : natural  := 64);
    Port ( 
           -- INPUTS
           clk   : in std_logic;
           reset : in std_logic;
           
           -- Architectural addr
           rs1_arch_addr_i    : in  STD_LOGIC_VECTOR (ARCH_ADDR_BITS-1 downto 0);
           rs2_arch_addr_i    : in  STD_LOGIC_VECTOR (ARCH_ADDR_BITS-1 downto 0);
           rd_arch_addr_i     : in  STD_LOGIC_VECTOR (ARCH_ADDR_BITS-1 downto 0);
           
           -- WB ports for physical_ready_bits register
           phys_ready_wb_we_i : in std_logic;
           wb_phys_addr_i     : in std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
           wb_rob_tag_i       : in std_logic_vector(TAG_WIDTH-1 downto 0);
           
           -- Physical addr
           rs1_phys_addr_o    : out STD_LOGIC_VECTOR (PHYS_ADDR_BITS-1 downto 0);
           rs2_phys_addr_o    : out STD_LOGIC_VECTOR (PHYS_ADDR_BITS-1 downto 0);
           rd_phys_addr_o     : out STD_LOGIC_VECTOR (PHYS_ADDR_BITS-1 downto 0);
           
           -- Operand ready bits
           rs1_ready_o        : out std_logic;
           rs2_ready_o        : out std_logic;
           
           -- Instruction tags
           rs1_tag_o          : out STD_LOGIC_VECTOR (TAG_WIDTH-1 downto 0);
           rs2_tag_o          : out STD_LOGIC_VECTOR (TAG_WIDTH-1 downto 0);
           
           -- ROB
           rob_write_en_o     : out std_logic;
           rename_rd_arch_o   : out std_logic;
           rename_rd_phys_o   : out std_logic;
           rename_prev_phys_o : out std_logic;
           rename_rd_instr_o  : out std_logic;
           
           wb_valid_i         : in std_logic;
           wb_rob_idx_i       : in std_logic_vector(TAG_WIDTH-1 downto 0);
           
           commit_valid_o     : out std_logic;
           commit_rd_arch_o   : out std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
           commit_rd_phys_o   : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
           commit_prev_phys_o : out std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
           commit_rd_instr_o  : out std_logic;
           
           -- Immediate
           imm_o              : out STD_LOGIC_VECTOR (DATA_WIDTH-1 downto 0);
           imm_i              : in  STD_LOGIC_VECTOR (DATA_WIDTH downto 0);
           
           -- Instruction type
           instr_type_i       : in  std_logic_vector (2 downto 0));
           
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
    signal push_data_s                  : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal pop_data_s                   : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    
    -- ROB signals
    signal rob_write_en_s               : std_logic;
    signal rob_rd_arch_s             : std_logic_vector(ARCH_ADDR_BITS-1 downto 0);
    signal rob_rd_phys_s             : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_prev_phys_s           : std_logic_vector(PHYS_ADDR_BITS-1 downto 0);
    signal rob_rd_instr_s            : std_logic;
    signal rob_tail_idx_s               : std_logic_vector(TAG_WIDTH-1 downto 0);    
    signal rob_full_s, rob_empty_s      : std_logic;
    
    signal phys_ready_reg               : std_logic_vector(NUM_PHYS_REGS-1 downto 0);
    signal phys_ready_next              : std_logic_vector(NUM_PHYS_REGS-1 downto 0);
    signal phys_ready_bit_rename_we_s   : std_logic;
    signal phys_ready_wb_we_s           : std_logic;
    
    -- Physical to tag memory
    type tag_array is array (0 to NUM_PHYS_REGS - 1) of std_logic_vector(TAG_WIDTH-1 downto 0);
    signal phys_to_tag : tag_array;
    
    -- Renaming table
    type rename_table_array is array (0 to 31) of std_logic_vector(5 downto 0); -- fizička adresa
    signal rename_table : rename_table_array;
    
    -- WE signals
    signal rename_table_we_s : std_logic;
    signal phys_tag_we_s     : std_logic;
    
begin
    
    imm_o <= imm_i;
    
    rs1_phys_addr_o <= rs1_phys_addr_s;
    rs2_phys_addr_o <= rs2_phys_addr_s;
    rd_phys_addr_o  <= rd_phys_addr_s;
    
    -- Register to keep track of the ready bits of the physical registers
    physical_ready_bits_reg:
    process (clk) is
    begin
      if (rising_edge(clk)) then
         if (reset = '0')then
            phys_ready_reg <= (others => '0');
         else
            phys_ready_reg <= phys_ready_next;
         end if;
      end if;
    end process; 
    
    physical_ready_bits_comb:
    process(phys_ready_bit_rename_we_s, phys_ready_wb_we_i, rd_phys_addr_s, wb_phys_addr_i, wb_rob_tag_i)
    begin
        phys_ready_next <= phys_ready_reg; -- podrazumevana vrednost zadržavanja
        for i in 0 to NUM_PHYS_REGS-1 loop
            if (phys_ready_bit_rename_we_s = '1' and (to_integer(unsigned(rd_phys_addr_s)) = i)) then
                phys_ready_next(i) <= '0';
            elsif (phys_ready_wb_we_i = '1' and (unsigned(wb_phys_addr_i) = i) and phys_to_tag(i) = wb_rob_tag_i) then
                phys_ready_next(i) <= '1';
            end if;
        end loop;
    end process;
    
    -- Renaming table
    rs1_phys_addr_s <= rename_table(to_integer(unsigned(rs1_arch_addr_i)));
    rs2_phys_addr_s <= rename_table(to_integer(unsigned(rs2_arch_addr_i)));
    
    write_rename_table:process(clk)
    begin
        if rising_edge(clk) then
            if (rename_table_we_s = '1') then
              rename_table(to_integer(unsigned(rd_arch_addr_i))) <= rd_phys_addr_s;
            end if;
        end if;
    end process;
    
    -- Tag memory
    rs1_tag_o <= phys_to_tag(to_integer(unsigned(rs1_phys_addr_s)));
    rs2_tag_o <= phys_to_tag(to_integer(unsigned(rs2_phys_addr_s)));
    
    write_tag:process(clk)
    begin
        if rising_edge(clk) then
            if (phys_tag_we_s = '1') then
                phys_to_tag(to_integer(unsigned(rd_phys_addr_s))) <= rob_tail_idx_s;
            end if;
        end if;
    end process;    
    
    -- ROB
    u_rob : entity work.ROB
        generic map (
            ROB_DEPTH => ROB_DEPTH
        )
        port map (
            clk               => clk,
            reset             => reset,
    
            -- Rename
            rob_write_en_i      => rob_write_en_s,   
            rename_rd_arch_i    => rob_rd_arch_s,   
            rename_rd_phys_i    => rob_rd_phys_s,   
            rename_prev_phys_i  => rob_prev_phys_s,
            rename_rd_instr_i   => rob_rd_instr_s, 
            rob_tail_idx_o      => rob_tail_idx_s,    
    
            -- WB
            wb_valid_i          => wb_valid_i,
            wb_rob_idx_i        => wb_rob_idx_i,
    
            -- Commit
            commit_valid_o      => commit_valid_o,
            commit_rd_arch_o    => commit_rd_arch_o,
            commit_rd_phys_o    => commit_rd_phys_o,
            commit_prev_phys_o  => commit_prev_phys_o,
            commit_rd_instr_o   => commit_rd_instr_o,
    
            rob_full_o          => rob_full_s,
            rob_empty_o         => rob_empty_s
        );
    
    -- Logic to generate proper inputs for the issue/dispatch stage
    comb_logic: process (rs1_phys_addr_s, rs2_phys_addr_s, phys_ready_reg, instr_type_i, pop_data_s) is
    begin
    
        rs1_ready_o     <= '0';
        rs2_ready_o     <= '0';
        
        rd_phys_addr_s  <= (others => '0');
        pop_s           <= '0';
        rename_table_we_s   <= '0';
        phys_tag_we_s       <= '0'; 
        phys_ready_bit_rename_we_s <= '0';
        
        case instr_type_i is
             when r_type_instruction | i_type_instruction | s_type_instruction | b_type_instruction =>    
                -- Rs1: Generate ready signal
                if(phys_ready_reg(to_integer(unsigned(rs1_phys_addr_s))) = '1') then
                    rs1_ready_o     <= '1';
                else
                    rs1_ready_o     <= '0';
                end if;
                
             when r_type_instruction | s_type_instruction | b_type_instruction | shamt_instruction =>
                -- Rs2: Generate ready signal
                if(phys_ready_reg(to_integer(unsigned(rs2_phys_addr_s))) = '1') then
                    rs2_ready_o     <= '1';
                else
                    rs2_ready_o     <= '0';
                end if;
                
             when r_type_instruction | i_type_instruction | u_type_instruction | j_type_instruction | shamt_instruction =>
                -- Rd: Read new physical address
                rd_phys_addr_s              <= pop_data_s;
                pop_s                       <= '1';
                rename_table_we_s           <= '1';  -- Write it in the rename table and 
                phys_tag_we_s               <= '1';  -- Write the new tag for the new physical register
                phys_ready_bit_rename_we_s  <= '1';  -- Reset the according bit in the physical_ready_bits register
                
                -- Write the entry for the ROB
                rob_write_en_s              <= '1';
                rob_rd_instr_s              <= '1'; 
                rob_rd_arch_s               <= rd_arch_addr_i;
                rob_rd_phys_s               <= pop_data_s;
                rob_prev_phys_s             <= rename_table(to_integer(unsigned( rd_arch_addr_i))); -- 
                
             when others =>
             
        end case;
    end process;     
    
    -- List of free physical registers (FIFO)
    u_free_list_fifo : entity work.free_list_fifo
    generic map (
        DEPTH => NUM_PHYS_REGS,
        WIDTH => TAG_WIDTH
    )
    port map (
        clk       => clk,
        reset     => reset,
        push      => push_s,
        push_data => push_data_s,
        pop       => pop_s,
        pop_data  => pop_data_s,
        empty     => empty_flag_s,
        full      => full_flag_s
    );
    
    -- First 64 cyclces fill in the free regs list
    fill_the_list: process(clk)
        variable i : integer := 0;
    begin
        if rising_edge(clk) then
            if reset = '1' then
                i := 0;
                push_s <= '0';
            elsif i < 64 then
                push_s <= '1';
                push_data_s <= std_logic_vector(to_unsigned(i, 6));
                i := i + 1;
            else
                push_s <= '0';
            end if;
        end if;
    end process; 
    
end Behavioral;

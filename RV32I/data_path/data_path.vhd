library ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

entity data_path is
   generic (DATA_WIDTH      : positive := 32;
            NUM_PHYS_REGS   : natural  := 64;
            TAG_WIDTH       : natural  := 6;
            ARCH_ADDR_BITS  : natural  := 5;
            PHYS_ADDR_BITS  : natural  := 5;
            ROB_DEPTH       : natural  := 64;
            MEM_BYTES       : natural  := 4 );  -- log2(DATA_WIDTH);
   port(
      clk                 : in  std_logic;
      reset               : in  std_logic;
      
      -- ********* Instruction Memory Interface (read-only) *********
      instr_mem_addr_o  : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Address of instruction
      instr_mem_data_i  : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- Instruction read from memory
      instruction_o     : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Forwarded instruction
      
      -- ********* Data Memory Interface (read/write) *********
      data_mem_addr_o   : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Address for data access
      data_mem_wdata_o  : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data to be written to memory
      data_mem_rdata_i  : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data read from memory
      data_mem_be_o     : out std_logic_vector(MEM_BYTES-1 downto 0);     -- Byte enable (load/store strobe)
      
      -- ********* Control signals ************************
      mem_to_reg_i        : in  std_logic;
      alu_op_i            : in  std_logic_vector(4 downto 0);
      pc_next_sel_i       : in  std_logic_vector(1 downto 0);
      alu_src_i           : in  std_logic;
      rd_we_i             : in  std_logic;
      funct3_i            : in  std_logic_vector(2 downto 0);
      load_instr_i        : in  std_logic;
      store_instr_i       : in  std_logic;   
      rd_data_source_i    : in  std_logic_vector(1 downto 0);   
      if_id_en_i          : in  std_logic;
      if_id_flush_i       : in  std_logic;
      
      -- ********* Status signals *************************
      branch_condition_o  : out std_logic
    -- ******************************************************
      );

end entity;

architecture Behavioral of data_path is
    
    -- Registers
    signal pc_reg_s, pc_next_s                  : std_logic_vector (DATA_WIDTH-1 downto 0);
    
    -- Signals
    signal instruction_if_s, instruction_id_s   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal pc_adder_s                           : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal branch_adder_s                       : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rs1_data_s, rs2_data_s, rd_data_s    : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal immediate_extended_id_s              : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal instruction_type_id_s                : std_logic_vector(2 downto 0);
    signal mem_rdata_extended_s                 : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rd_data_wb                           : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- ALU signals   
    signal alu_zero_s, alu_of_o_s : std_logic;
    signal b_s, a_s               : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal alu_result_s           : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    signal instruction_type_rn_s   : std_logic_vector(2 downto 0);
    signal immediate_extended_rn_s : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Renaming signals
    signal rs1_arch_addr_s      : std_logic_vector(4 downto 0);
    signal rs2_arch_addr_s      : std_logic_vector(4 downto 0);
    signal rd_arch_addr_s       : std_logic_vector(4 downto 0);
    
    signal phys_ready_wb_we_s   : std_logic;
    signal wb_phys_addr_s       : std_logic_vector(4 downto 0);
    signal wb_rob_tag_s         : std_logic_vector(5 downto 0);
    
    signal rs1_phys_addr_s      : std_logic_vector(4 downto 0);
    signal rs2_phys_addr_s      : std_logic_vector(4 downto 0);
    signal rd_phys_addr_s       : std_logic_vector(4 downto 0);
    
    signal rs1_ready_s          : std_logic;
    signal rs2_ready_s          : std_logic;
    
    signal rs1_tag_s            : std_logic_vector(5 downto 0);
    signal rs2_tag_s            : std_logic_vector(5 downto 0);
    
    signal rob_write_en_s       : std_logic;
    signal rename_rd_arch_s     : std_logic;
    signal rename_rd_phys_s     : std_logic;
    signal rename_prev_phys_s   : std_logic;
    signal rename_rd_instr_s    : std_logic;
    
    signal wb_valid_s           : std_logic;
    signal wb_rob_idx_s         : std_logic_vector(5 downto 0);
    
    signal commit_valid_s       : std_logic;
    signal commit_rd_arch_s     : std_logic_vector(4 downto 0);
    signal commit_rd_phys_s     : std_logic_vector(4 downto 0);
    signal commit_prev_phys_s   : std_logic_vector(4 downto 0);
    signal commit_rd_instr_s    : std_logic;
    
    signal imm_s              : std_logic_vector(32 downto 0);

begin
    
   --***********INSTRUCTION FETCH STAGE**********************   
   -- Program counter register
   pc_reg : process (clk) is
   begin
      if (rising_edge(clk)) then
         if (reset = '0')then
            pc_reg_s <= (others => '0');
         else
            pc_reg_s <= pc_next_s;
         end if;
      end if;
   end process;  
   
   -- Next sequential instruction (pc+4)
   pc_adder_s     <= std_logic_vector(unsigned(pc_reg_s) + to_unsigned(4, DATA_WIDTH));

   -- MUX koji odredjuje sledecu vrednost za programski brojac.
   -- Ako se ne desi skok programski brojac se uvecava za 4.
   with pc_next_sel_i select
      pc_next_s <= 
      pc_adder_s              when "00",   -- PC+4
      branch_adder_s          when "01",   -- BRANCH instr
      alu_result_s            when "10",   -- JALR instr
      pc_adder_s              when others; -- PC+4
      
   --*****************************************************
     
   --***********INSTRUCTION DECODE STAGE**********************   
   --IF/ID register
    if_id : process (clk) is
    begin
        if (rising_edge(clk)) then
            if(if_id_en_i = '1')then
                if (reset = '0' or if_id_flush_i = '1')then
                    instruction_id_s <= (others => '0');
                else
                    instruction_id_s <= instruction_if_s;
                end if;
            end if;
        end if;
    end process;
    
   -- b type jumps singal
   branch_adder_s <= std_logic_vector(unsigned(immediate_extended_id_s) + unsigned(pc_reg_s));
    
   -- Modul za prosirenje immediate polja instrukcije
   immediate_1 : entity work.immediate
        port map (
           instruction_i        => instruction_id_s,
           immediate_extended_o => immediate_extended_id_s,
           instruction_type_o   => instruction_type_id_s
           );
           
   --*****************************************************
    
   --***********REGISTER RENAMING STAGE**********************   
    --ID/RN register
    id_ex : process (clk) is
    begin
        if (rising_edge(clk)) then
            if (reset = '0')then
                rs1_arch_addr_s         <= (others => '0');
                rs2_arch_addr_s         <= (others => '0');
                rd_arch_addr_s          <= (others => '0');
                immediate_extended_rn_s <= (others => '0');
                instruction_type_rn_s   <= (others => '0');
                
            else
                rs1_arch_addr_s         <= instruction_id_s(19 downto 15);
                rs2_arch_addr_s         <= instruction_id_s(24 downto 20);
                rd_arch_addr_s          <= instruction_id_s(11 downto 7);
                immediate_extended_rn_s <= immediate_extended_id_s;
                instruction_type_rn_s   <= instruction_type_id_s;
            end if;
        end if;
    end process;

    u_renaming_module: entity work.renaming_module
      generic map (
        DATA_WIDTH      => DATA_WIDTH,
        NUM_PHYS_REGS   => NUM_PHYS_REGS,
        TAG_WIDTH       => TAG_WIDTH,
        ARCH_ADDR_BITS  => ARCH_ADDR_BITS,
        PHYS_ADDR_BITS  => PHYS_ADDR_BITS,
        ROB_DEPTH       => ROB_DEPTH     
      )
      port map (
        clk                   => clk,
        reset                 => reset,
    
        rs1_arch_addr_i       => rs1_arch_addr_s,
        rs2_arch_addr_i       => rs2_arch_addr_s,
        rd_arch_addr_i        => rd_arch_addr_s,
    
        phys_ready_wb_we_i    => phys_ready_wb_we_s,
        wb_phys_addr_i        => wb_phys_addr_s,
        wb_rob_tag_i          => wb_rob_tag_s,
    
        rs1_phys_addr_o       => rs1_phys_addr_s,
        rs2_phys_addr_o       => rs2_phys_addr_s,
        rd_phys_addr_o        => rd_phys_addr_s,
    
        rs1_ready_o           => rs1_ready_s,
        rs2_ready_o           => rs2_ready_s,
    
        rs1_tag_o             => rs1_tag_s,
        rs2_tag_o             => rs2_tag_s,
    
        rob_write_en_o        => rob_write_en_s,
        rename_rd_arch_o      => rename_rd_arch_s,
        rename_rd_phys_o      => rename_rd_phys_s,
        rename_prev_phys_o    => rename_prev_phys_s,
        rename_rd_instr_o     => rename_rd_instr_s,
    
        wb_valid_i            => wb_valid_s,
        wb_rob_idx_i          => wb_rob_idx_s,
    
        commit_valid_o        => commit_valid_s,
        commit_rd_arch_o      => commit_rd_arch_s,
        commit_rd_phys_o      => commit_rd_phys_s,
        commit_prev_phys_o    => commit_prev_phys_s,
        commit_rd_instr_o     => commit_rd_instr_s,
    
        imm_o                 => imm_s,
        imm_i                 => immediate_extended_rn_s,
    
        instr_type_i          => instruction_type_rn_s
      );


    
    
   --*****************************************************




















   --Registarska banka
   register_bank_1 : entity work.register_bank
        generic map (
           WIDTH => 32)
        port map (
           clk           => clk,
           reset         => reset,
           rd_we_i       => rd_we_i,
           rs1_address_i => instruction_if_s (19 downto 15), -- rename?
           rs2_address_i => instruction_if_s (24 downto 20), -- rename?
           rs1_data_o    => rs1_data_s,
           rs2_data_o    => rs2_data_s,
           rd_address_i  => instruction_if_s (11 downto 7), -- rename?
           rd_data_i     => rd_data_s);

   -- Aritmeticko logicka jedinica
   ALU_1 : entity work.ALU
        generic map (
           WIDTH => DATA_WIDTH)
        port map (
           a_i    => a_s,
           b_i    => b_s,
           op_i   => alu_op_i,
           res_o  => alu_result_s,
           zero_o => alu_zero_s,
           of_o   => alu_of_o_s);
         
    LSU_1 : entity work.LSU
        generic map (
          DATA_WIDTH => 32
        )
        port map ( 
          data_mem_addr_i   => alu_result_s,
          data_mem_wdata_i  => rs2_data_s,
          data_mem_rdata_o  => mem_rdata_extended_s,
          
          data_mem_addr_o   => data_mem_addr_o,
          data_mem_wdata_o  => data_mem_wdata_o,
          data_mem_rdata_i  => data_mem_rdata_i,
          data_mem_be_o     => data_mem_be_o,
 
          funct3_i          => funct3_i,
          load_instr_i      => load_instr_i,
          store_instr_i     => store_instr_i
        );

   -- Branch decision unit
   branch_decision_1 : entity work.branch_decision_unit
    generic map (
        DATA_WIDTH => 32
      )
      port map (
        branch_condition_o => branch_condition_o,
        a_i                => rs1_data_s,
        b_i                => rs2_data_s,
        funct3_i           => instruction_if_s(14 downto 12) -- decode/rename?
        -- opcode              
      );

   --*****************************************************

   --***********I/O******************************
   
   -- Controlpath
   instruction_o    <= instruction_if_s;
   
   -- Instruction memory
   instruction_if_s    <= instr_mem_data_i;
   instr_mem_addr_o <= pc_reg_s;
   
   --***********Kombinaciona logika***********************

   -- MUX koji odredjuje sledecu vrednost za b ulaz ALU jedinice.
   b_s <= rs2_data_s when alu_src_i = '0' else
          immediate_extended_s;
   -- Azuriranje a ulaza ALU jedinice
   a_s <= rs1_data_s;

   -- MUX for selecting which data gets written (rd_data_wb or pc + 4)
    with rd_data_source_i select 
        rd_data_s <= 
        rd_data_wb              when "00", -- wb
        pc_adder_s              when "01", -- PC+4, JAL and JALR instr
        immediate_extended_s    when "10", -- LUI instr
        branch_adder_s          when "11", -- AUIPC instr
        rd_data_wb              when others ;
   
   -- MUX for selecting which data goes to write-back (alu result or extended data from memory)
   rd_data_wb <= mem_rdata_extended_s when mem_to_reg_i = '1' else
                alu_result_s;
   
   --*****************************************************
   
end architecture;



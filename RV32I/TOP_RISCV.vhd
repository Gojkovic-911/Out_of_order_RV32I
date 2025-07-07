library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity TOP_RISCV is
   generic (DATA_WIDTH : positive := 32;
            MEM_BYTES  : positive := 4);
   port(
      -- ********* Globalna sinhronizacija ******************
      clk                 : in  std_logic;
      reset               : in  std_logic;
      -- ********* Interfejs ka Memoriji za instrukcije *****
      instr_mem_addr_o : out std_logic_vector (31 downto 0);
      instr_mem_data_i    : in  std_logic_vector(31 downto 0);
      -- ********* Interfejs ka Memoriji za podatke *********
      data_mem_be_o       : out std_logic_vector(MEM_BYTES-1 downto 0);
      data_mem_addr_o     : out std_logic_vector(31 downto 0);
      data_mem_wdata_o    : out std_logic_vector(31 downto 0);
      data_mem_rdata_i    : in  std_logic_vector (31 downto 0));
end entity;

architecture structural of TOP_RISCV is
   signal instruction_s      : std_logic_vector(31 downto 0);
   signal mem_to_reg_s       : std_logic;
   signal alu_op_s           : std_logic_vector (4 downto 0);
   signal alu_src_s          : std_logic;
   signal rd_we_s            : std_logic;
   signal branch_condition_s : std_logic;
   signal pc_next_sel_s      : std_logic_vector (1 downto 0);
   signal funct3_s           : std_logic_vector (2 downto 0);
   signal load_instr_s       : std_logic;
   signal store_instr_s      : std_logic;
   signal rd_data_source_s   : std_logic_vector(1 downto 0);
   
begin

   data_path_1 : entity work.data_path
      generic map (
         DATA_WIDTH => DATA_WIDTH)
      port map (
         clk                 => clk,
         reset               => reset,
         instr_mem_addr_o    => instr_mem_addr_o,
         instr_mem_data_i    => instr_mem_data_i,
         instruction_o       => instruction_s,
         data_mem_addr_o     => data_mem_addr_o,
         data_mem_wdata_o    => data_mem_wdata_o,
         data_mem_rdata_i    => data_mem_rdata_i,
         data_mem_be_o       => data_mem_be_o,
         mem_to_reg_i        => mem_to_reg_s,
         alu_op_i            => alu_op_s,
         pc_next_sel_i       => pc_next_sel_s,
         alu_src_i           => alu_src_s,
         rd_we_i             => rd_we_s,
         funct3_i            => funct3_s,
         load_instr_i        => load_instr_s,
         store_instr_i       => store_instr_s,
         branch_condition_o  => branch_condition_s,
         rd_data_source_i          => rd_data_source_s
         );

   control_path_1 : entity work.control_path
      port map (
         clk                => clk,
         reset              => reset,
         instruction_i      => instruction_s,
         mem_to_reg_o       => mem_to_reg_s,
         alu_op_o           => alu_op_s,
         pc_next_sel_o      => pc_next_sel_s,
         alu_src_o          => alu_src_s,
         rd_we_o            => rd_we_s,
         funct3_o           => funct3_s,
         load_instr_o       => load_instr_s,
         store_instr_o      => store_instr_s,
         branch_condition_i => branch_condition_s,
         rd_data_source_o          => rd_data_source_s
         );

end architecture;



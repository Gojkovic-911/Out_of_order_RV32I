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
      data_mem_rdata_i    : in  std_logic_vector (31 downto 0)
      
      );
end entity;
    
architecture structural of TOP_RISCV is
    signal instruction_s      : std_logic_vector(31 downto 0);
    
    signal decode_instr_format_s    : std_logic_vector(2 downto 0);
    
    signal dispatch_instr_type_s    : std_logic_vector(3 downto 0);
    signal dispatch_instr_subtype_s  : std_logic_vector(4 downto 0);
    
    signal stall_if_s               : std_logic;
    signal stall_id_s               : std_logic;
    signal stall_rn_s               : std_logic;
    signal stall_is_s               : std_logic;
    signal stall_ex_s               : std_logic;
    signal stall_iq_is_s            : std_logic;
    signal if_id_flush_s            : std_logic;
    
    signal rename_rd_we_s           : std_logic;
    signal rename_rs1_used_s        : std_logic;
    signal rename_rs2_used_s        : std_logic;
    signal rename_is_jump_s         : std_logic;
    
    signal rob_full_s               : std_logic;
    signal free_list_fifo_empty_s   : std_logic;
    signal iq_full_s                : std_logic;
    
    signal rs_full_s                : std_logic;    -- reservation station full
    signal branch_taken_s           : std_logic;    -- Branch taken execute stage
    
    
    
begin
    
    data_path_1 : entity work.data_path
        generic map (
            DATA_WIDTH => DATA_WIDTH)
        port map (
            clk                      => clk,
            reset                    => reset,
            
            instr_mem_addr_o         => instr_mem_addr_o,
            instr_mem_data_i         => instr_mem_data_i,
            instruction_o            => instruction_s,
            
            data_mem_addr_o          => data_mem_addr_o,
            data_mem_wdata_o         => data_mem_wdata_o,
            data_mem_rdata_i         => data_mem_rdata_i,
            data_mem_be_o            => data_mem_be_o,
            
            decode_instr_format_i    => decode_instr_format_s,
            
            rename_rd_we_i           => rename_rd_we_s,
            rename_rs1_used_i        => rename_rs1_used_s,
            rename_rs2_used_i        => rename_rs2_used_s,
            rename_is_jump_i         => rename_is_jump_s,
            
            dispatch_instr_type_i    => dispatch_instr_type_s,
            dispatch_instr_subtype_i => dispatch_instr_subtype_s,
            
            stall_if_i               => stall_if_s,
            stall_id_i               => stall_id_s,
            stall_rn_i               => stall_rn_s,
            stall_is_i               => stall_is_s,
            stall_ex_i               => stall_ex_s,
            stall_iq_is_i            => stall_iq_is_s,
            
            if_id_flush_i            => if_id_flush_s,
            
            rob_full_o               => rob_full_s,
            free_list_fifo_empty_o   => free_list_fifo_empty_s,
            iq_full_o                => iq_full_s,
            
            rs_full_o               => rs_full_s,
            branch_taken_o          => branch_taken_s
            );

    control_path_1 : entity work.control_path
        port map (
            clk                      => clk,
            reset                    => reset,
            
            instruction_i            => instruction_s,
            instr_format_o           => decode_instr_format_s,
            
            rename_rd_we_o           => rename_rd_we_s,
            rename_rs1_used_o        => rename_rs1_used_s,
            rename_rs2_used_o        => rename_rs2_used_s,
            rename_is_jump_o         => rename_is_jump_s,
            
            dispatch_instr_type_o    => dispatch_instr_type_s,
            dispatch_instr_subtype_o => dispatch_instr_subtype_s,
            
            rob_full_i               => rob_full_s,
            free_list_fifo_empty_i   => free_list_fifo_empty_s,
            iq_full_i                => iq_full_s,
         
            rs_full_i               => rs_full_s,
            branch_taken_i          => branch_taken_s,
            
            stall_if_o               => stall_if_s,
            stall_id_o               => stall_id_s,
            stall_rn_o               => stall_rn_s,
            stall_is_o               => stall_is_s,
            stall_ex_o               => stall_ex_s,
            stall_iq_is_o            => stall_iq_is_s
            );

end architecture;



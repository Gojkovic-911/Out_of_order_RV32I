library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity riscv_top is
   generic (DATA_WIDTH     : positive := 32;
            MEM_BYTES      : positive := 4;
            ARCH_ADDR_BITS : positive := 5;
            NUM_PHYS_REGS  : positive := 64;
            ROB_DEPTH      : positive := 64;
            IQ_DEPTH       : positive := 16;
            LSQ_DEPTH      : positive := 16;
            RF_RAM_STYLE   : string   := "auto";
            IQ_RAM_STYLE   : string   := "distributed");
   port(
      -- Clock and reset
      clk                 : in  std_logic;
      reset               : in  std_logic;
      -- Instruction memory port
      instr_mem_addr_o    : out std_logic_vector (31 downto 0);
      instr_mem_data_i    : in  std_logic_vector(31 downto 0);
      -- Data memory port: request valid/ready, response pulse
      data_mem_req_o      : out std_logic;
      data_mem_ready_i    : in  std_logic;
      data_mem_we_o       : out std_logic;
      data_mem_addr_o     : out std_logic_vector(31 downto 0);
      data_mem_wdata_o    : out std_logic_vector(31 downto 0);
      data_mem_be_o       : out std_logic_vector(MEM_BYTES-1 downto 0);
      data_mem_rsp_i      : in  std_logic;
      data_mem_rdata_i    : in  std_logic_vector(31 downto 0)
      );
end entity;
    
architecture structural of riscv_top is
    -- Index widths from depths
    constant PHYS_ADDR_BITS : natural := integer(ceil(log2(real(NUM_PHYS_REGS))));
    constant ROB_ADDR_BITS  : natural := integer(ceil(log2(real(ROB_DEPTH))));
    constant IQ_BITS        : natural := integer(ceil(log2(real(IQ_DEPTH))));
    constant LSQ_BITS       : natural := integer(ceil(log2(real(LSQ_DEPTH))));

    signal instruction_s      : std_logic_vector(31 downto 0);
    
    signal decode_instr_format_s    : std_logic_vector(2 downto 0);
    signal fetch_is_jump_s         : std_logic;
    signal fetch_is_jalr_s         : std_logic;
    
    signal dispatch_instr_type_s    : std_logic_vector(3 downto 0);
    signal dispatch_instr_subtype_s  : std_logic_vector(4 downto 0);
    
    signal stall_if_s               : std_logic;
    signal stall_id_s               : std_logic;
    signal stall_rn_s               : std_logic;
    signal stall_is_s               : std_logic;
    signal stall_ex_s               : std_logic;
    signal stall_iq_is_s            : std_logic;
    
    signal rename_rd_we_s           : std_logic;
    signal rename_rs1_used_s        : std_logic;
    signal rename_rs2_used_s        : std_logic;
    
    signal rob_full_s               : std_logic;
    signal lsq_full_s               : std_logic;
    signal free_list_fifo_empty_s   : std_logic;
    signal iq_full_s                : std_logic;
    
    signal ex_busy_s                : std_logic;
    signal misspredict_s            : std_logic;    
    signal pc_next_sel_s            : std_logic;    
    signal flush_pipe_s             : std_logic;
    signal if_id_flush_s            : std_logic;

begin
    
    data_path_1 : entity work.data_path
        generic map (
            DATA_WIDTH     => DATA_WIDTH,
            MEM_BYTES      => MEM_BYTES,
            ARCH_ADDR_BITS => ARCH_ADDR_BITS,
            NUM_PHYS_REGS  => NUM_PHYS_REGS,
            PHYS_ADDR_BITS => PHYS_ADDR_BITS,
            ROB_DEPTH      => ROB_DEPTH,
            ROB_ADDR_BITS  => ROB_ADDR_BITS,
            IQ_DEPTH       => IQ_DEPTH,
            IQ_BITS        => IQ_BITS,
            LSQ_DEPTH      => LSQ_DEPTH,
            LSQ_BITS       => LSQ_BITS,
            RF_RAM_STYLE   => RF_RAM_STYLE,
            IQ_RAM_STYLE   => IQ_RAM_STYLE)
        port map (
            clk                      => clk,
            reset                    => reset,
            
            instr_mem_addr_o         => instr_mem_addr_o,
            instr_mem_data_i         => instr_mem_data_i,
            instruction_o            => instruction_s,
            
            data_mem_req_o           => data_mem_req_o,
            data_mem_ready_i         => data_mem_ready_i,
            data_mem_we_o            => data_mem_we_o,
            data_mem_rsp_i           => data_mem_rsp_i,
            data_mem_addr_o          => data_mem_addr_o,
            data_mem_wdata_o         => data_mem_wdata_o,
            data_mem_rdata_i         => data_mem_rdata_i,
            data_mem_be_o            => data_mem_be_o,
            
            decode_instr_format_i    => decode_instr_format_s,
            fetch_is_jump_i         => fetch_is_jump_s,
            fetch_is_jalr_i         => fetch_is_jalr_s,
            
            rename_rd_we_i           => rename_rd_we_s,
            rename_rs1_used_i        => rename_rs1_used_s,
            rename_rs2_used_i        => rename_rs2_used_s,
            
            dispatch_instr_type_i    => dispatch_instr_type_s,
            dispatch_instr_subtype_i => dispatch_instr_subtype_s,
            
            stall_if_i               => stall_if_s,
            stall_id_i               => stall_id_s,
            stall_rn_i               => stall_rn_s,
            stall_is_i               => stall_is_s,
            stall_ex_i               => stall_ex_s,
            stall_iq_is_i            => stall_iq_is_s,
            
            pc_next_sel_i            => pc_next_sel_s,
            flush_pipe_i             => flush_pipe_s,
            if_id_flush_i            => if_id_flush_s,
            
            rob_full_o               => rob_full_s,
            free_list_fifo_empty_o   => free_list_fifo_empty_s,
            iq_full_o                => iq_full_s,
            misspredict_o            => misspredict_s, 
            
            ex_busy_o               => ex_busy_s,
            lsq_full_o              => lsq_full_s
            );

    control_path_1 : entity work.control_path
        port map (
            clk                      => clk,
            reset                    => reset,
            
            instruction_i            => instruction_s,
            instr_format_o           => decode_instr_format_s,
            
            fetch_is_jump_o         => fetch_is_jump_s,
            fetch_is_jalr_o         => fetch_is_jalr_s,
            
            rename_rd_we_o           => rename_rd_we_s,
            rename_rs1_used_o        => rename_rs1_used_s,
            rename_rs2_used_o        => rename_rs2_used_s,
            pc_next_sel_o            => pc_next_sel_s,
            
            dispatch_instr_type_o    => dispatch_instr_type_s,
            dispatch_instr_subtype_o => dispatch_instr_subtype_s,
            
            rob_full_i               => rob_full_s,
            free_list_fifo_empty_i   => free_list_fifo_empty_s,
            iq_full_i                => iq_full_s,
         
            ex_busy_i                => ex_busy_s,
            lsq_full_i               => lsq_full_s,
            misspredict_i            => misspredict_s, 
            
            stall_if_o               => stall_if_s,
            stall_id_o               => stall_id_s,
            stall_rn_o               => stall_rn_s,
            stall_is_o               => stall_is_s,
            stall_ex_o               => stall_ex_s,
            stall_iq_is_o            => stall_iq_is_s,
            flush_pipe_o             => flush_pipe_s,
            if_id_flush_o            => if_id_flush_s
            );

end architecture;

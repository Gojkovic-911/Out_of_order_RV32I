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
      -- AXI4-Lite data port
      m_axi_awaddr        : out std_logic_vector(31 downto 0);
      m_axi_awprot        : out std_logic_vector(2 downto 0);
      m_axi_awvalid       : out std_logic;
      m_axi_awready       : in  std_logic;
      m_axi_wdata         : out std_logic_vector(31 downto 0);
      m_axi_wstrb         : out std_logic_vector(3 downto 0);
      m_axi_wvalid        : out std_logic;
      m_axi_wready        : in  std_logic;
      m_axi_bresp         : in  std_logic_vector(1 downto 0);
      m_axi_bvalid        : in  std_logic;
      m_axi_bready        : out std_logic;
      m_axi_araddr        : out std_logic_vector(31 downto 0);
      m_axi_arprot        : out std_logic_vector(2 downto 0);
      m_axi_arvalid       : out std_logic;
      m_axi_arready       : in  std_logic;
      m_axi_rdata         : in  std_logic_vector(31 downto 0);
      m_axi_rresp         : in  std_logic_vector(1 downto 0);
      m_axi_rvalid        : in  std_logic;
      m_axi_rready        : out std_logic
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

    -- Core data port
    signal dmem_req_s               : std_logic;
    signal dmem_ready_s             : std_logic;
    signal dmem_we_s                : std_logic;
    signal dmem_rsp_s               : std_logic;
    signal dmem_be_s                : std_logic_vector(MEM_BYTES-1 downto 0);
    signal dmem_addr_s              : std_logic_vector(31 downto 0);
    signal dmem_wdata_s             : std_logic_vector(31 downto 0);
    signal dmem_rdata_s             : std_logic_vector(31 downto 0);

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
            
            data_mem_req_o           => dmem_req_s,
            data_mem_ready_i         => dmem_ready_s,
            data_mem_we_o            => dmem_we_s,
            data_mem_rsp_i           => dmem_rsp_s,
            data_mem_addr_o          => dmem_addr_s,
            data_mem_wdata_o         => dmem_wdata_s,
            data_mem_rdata_i         => dmem_rdata_s,
            data_mem_be_o            => dmem_be_s,
            
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

    -- Data port to AXI
    axi_bridge : entity work.axi_lite_master
        port map (
            clk           => clk,
            reset         => reset,
            req_valid_i   => dmem_req_s,
            req_ready_o   => dmem_ready_s,
            req_we_i      => dmem_we_s,
            req_addr_i    => dmem_addr_s,
            req_wdata_i   => dmem_wdata_s,
            req_be_i      => dmem_be_s,
            rsp_valid_o   => dmem_rsp_s,
            rsp_rdata_o   => dmem_rdata_s,
            m_axi_awaddr  => m_axi_awaddr,
            m_axi_awprot  => m_axi_awprot,
            m_axi_awvalid => m_axi_awvalid,
            m_axi_awready => m_axi_awready,
            m_axi_wdata   => m_axi_wdata,
            m_axi_wstrb   => m_axi_wstrb,
            m_axi_wvalid  => m_axi_wvalid,
            m_axi_wready  => m_axi_wready,
            m_axi_bresp   => m_axi_bresp,
            m_axi_bvalid  => m_axi_bvalid,
            m_axi_bready  => m_axi_bready,
            m_axi_araddr  => m_axi_araddr,
            m_axi_arprot  => m_axi_arprot,
            m_axi_arvalid => m_axi_arvalid,
            m_axi_arready => m_axi_arready,
            m_axi_rdata   => m_axi_rdata,
            m_axi_rresp   => m_axi_rresp,
            m_axi_rvalid  => m_axi_rvalid,
            m_axi_rready  => m_axi_rready);

end architecture;

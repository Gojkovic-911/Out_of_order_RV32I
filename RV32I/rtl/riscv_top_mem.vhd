library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Core for the board: instruction memory inside, data accesses on an
-- AXI4-Lite master port (data memory and peripherals on the interconnect)
entity riscv_top_mem is
   generic
           (DATA_WIDTH  : positive := 32;
            MEM_BYTES   : positive := 4;
            WADDR       : natural := 14);  -- 16 KB instruction memory
   port(
      -- ********* Global clock and reset ******************
      clk                 : in  std_logic;
      reset               : in  std_logic;    -- active low

      -- ********* AXI4-Lite master (data) ******************
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

architecture rtl of riscv_top_mem is

   signal instr_addr    : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal instr_rdata   : std_logic_vector(DATA_WIDTH-1  downto 0);

   signal dmem_req      : std_logic;
   signal dmem_ready    : std_logic;
   signal dmem_we       : std_logic;
   signal dmem_rsp      : std_logic;
   signal dmem_be       : std_logic_vector(MEM_BYTES-1   downto 0);
   signal dmem_addr     : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal dmem_wdata    : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal dmem_rdata    : std_logic_vector(DATA_WIDTH-1  downto 0);

begin

   CPU: entity work.TOP_RISCV
      generic map (
         DATA_WIDTH => 32,
         MEM_BYTES  => 4
      )
      port map (
         clk               => clk,
         reset             => reset,
         instr_mem_addr_o  => instr_addr,
         instr_mem_data_i  => instr_rdata,
         data_mem_req_o    => dmem_req,
         data_mem_ready_i  => dmem_ready,
         data_mem_we_o     => dmem_we,
         data_mem_rsp_i    => dmem_rsp,
         data_mem_be_o     => dmem_be,
         data_mem_addr_o   => dmem_addr,
         data_mem_wdata_o  => dmem_wdata,
         data_mem_rdata_i  => dmem_rdata
      );

   -- Program is loaded at synthesis (initialization still to be added)
   INSTR_MEM: entity work.BRAM
      generic map (
         WADDR => WADDR
      )
      port map (
         clk        => clk,
         en_a_i     => '1',
         en_b_i     => '0',
         data_a_i   => (others => '0'),
         data_b_i   => (others => '0'),
         addr_a_i   => instr_addr(WADDR-1 downto 0),
         addr_b_i   => (others => '0'),
         we_a_i     => "0000",
         we_b_i     => "0000",
         data_a_o   => instr_rdata,
         data_b_o   => open
      );

   AXI_BRIDGE: entity work.axi_lite_master
      port map (
         clk           => clk,
         reset         => reset,
         req_valid_i   => dmem_req,
         req_ready_o   => dmem_ready,
         req_we_i      => dmem_we,
         req_addr_i    => dmem_addr,
         req_wdata_i   => dmem_wdata,
         req_be_i      => dmem_be,
         rsp_valid_o   => dmem_rsp,
         rsp_rdata_o   => dmem_rdata,
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
         m_axi_rready  => m_axi_rready
      );

end rtl;

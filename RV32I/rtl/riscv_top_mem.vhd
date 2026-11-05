library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Core with instruction and data memory, AXI4-Lite port for the rest of the address space
entity riscv_top_mem is
   generic
           (DATA_WIDTH     : positive := 32;
            MEM_BYTES      : positive := 4;
            ARCH_ADDR_BITS : positive := 5;
            NUM_PHYS_REGS  : positive := 64;
            ROB_DEPTH      : positive := 64;
            IQ_DEPTH       : positive := 16;
            LSQ_DEPTH      : positive := 16;
            -- Instruction memory of 2^IMEM_ADDR_BITS bytes
            IMEM_ADDR_BITS : natural  := 14;
            -- Data memory: 2^DMEM_ADDR_BITS bytes at DMEM_BASE (below 2^31), read latency 1 or 2
            DMEM_BASE       : natural  := 0;
            DMEM_ADDR_BITS  : natural  := 14;
            DMEM_RD_LATENCY : positive := 1;
            -- Data memory: MIN..MAX random extra wait per access (simulation, 0..0 in hardware)
            DATA_MEM_MIN_WAIT : natural := 0;
            DATA_MEM_MAX_WAIT : natural := 0;
            -- Vivado ram_style of the register file and IQ payload
            RF_RAM_STYLE   : string   := "auto";
            IQ_RAM_STYLE   : string   := "distributed";
            -- Program and data loaded at synthesis
            IMEM_INIT_FILE : string  := "";
            DMEM_INIT_FILE : string  := "";
            -- Simulation: reload both files on the first clock
            SIM_LOAD       : boolean := false);
   port(
      -- Clock and reset
      clk                 : in  std_logic;
      reset               : in  std_logic;
      -- Reset of the AXI slave ports, apart from the core (JTAG loads while the core is held)
      s_axi_aresetn       : in  std_logic;

      -- AXI4 slave on port B of the instruction memory
      s_axi_imem_awaddr  : in  std_logic_vector(IMEM_ADDR_BITS-1 downto 0);
      s_axi_imem_awlen   : in  std_logic_vector(7 downto 0);
      s_axi_imem_awsize  : in  std_logic_vector(2 downto 0);
      s_axi_imem_awburst : in  std_logic_vector(1 downto 0);
      s_axi_imem_awvalid : in  std_logic;
      s_axi_imem_awready : out std_logic;
      s_axi_imem_wdata   : in  std_logic_vector(31 downto 0);
      s_axi_imem_wstrb   : in  std_logic_vector(3 downto 0);
      s_axi_imem_wlast   : in  std_logic;
      s_axi_imem_wvalid  : in  std_logic;
      s_axi_imem_wready  : out std_logic;
      s_axi_imem_bresp   : out std_logic_vector(1 downto 0);
      s_axi_imem_bvalid  : out std_logic;
      s_axi_imem_bready  : in  std_logic;
      s_axi_imem_araddr  : in  std_logic_vector(IMEM_ADDR_BITS-1 downto 0);
      s_axi_imem_arlen   : in  std_logic_vector(7 downto 0);
      s_axi_imem_arsize  : in  std_logic_vector(2 downto 0);
      s_axi_imem_arburst : in  std_logic_vector(1 downto 0);
      s_axi_imem_arvalid : in  std_logic;
      s_axi_imem_arready : out std_logic;
      s_axi_imem_rdata   : out std_logic_vector(31 downto 0);
      s_axi_imem_rresp   : out std_logic_vector(1 downto 0);
      s_axi_imem_rlast   : out std_logic;
      s_axi_imem_rvalid  : out std_logic;
      s_axi_imem_rready  : in  std_logic;
      -- AXI4 slave on port B of the data memory
      s_axi_dmem_awaddr  : in  std_logic_vector(DMEM_ADDR_BITS-1 downto 0);
      s_axi_dmem_awlen   : in  std_logic_vector(7 downto 0);
      s_axi_dmem_awsize  : in  std_logic_vector(2 downto 0);
      s_axi_dmem_awburst : in  std_logic_vector(1 downto 0);
      s_axi_dmem_awvalid : in  std_logic;
      s_axi_dmem_awready : out std_logic;
      s_axi_dmem_wdata   : in  std_logic_vector(31 downto 0);
      s_axi_dmem_wstrb   : in  std_logic_vector(3 downto 0);
      s_axi_dmem_wlast   : in  std_logic;
      s_axi_dmem_wvalid  : in  std_logic;
      s_axi_dmem_wready  : out std_logic;
      s_axi_dmem_bresp   : out std_logic_vector(1 downto 0);
      s_axi_dmem_bvalid  : out std_logic;
      s_axi_dmem_bready  : in  std_logic;
      s_axi_dmem_araddr  : in  std_logic_vector(DMEM_ADDR_BITS-1 downto 0);
      s_axi_dmem_arlen   : in  std_logic_vector(7 downto 0);
      s_axi_dmem_arsize  : in  std_logic_vector(2 downto 0);
      s_axi_dmem_arburst : in  std_logic_vector(1 downto 0);
      s_axi_dmem_arvalid : in  std_logic;
      s_axi_dmem_arready : out std_logic;
      s_axi_dmem_rdata   : out std_logic_vector(31 downto 0);
      s_axi_dmem_rresp   : out std_logic_vector(1 downto 0);
      s_axi_dmem_rlast   : out std_logic;
      s_axi_dmem_rvalid  : out std_logic;
      s_axi_dmem_rready  : in  std_logic;

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

architecture rtl of riscv_top_mem is

   -- IP Integrator interface attributes
   attribute X_INTERFACE_INFO      : string;
   attribute X_INTERFACE_PARAMETER : string;
   attribute X_INTERFACE_INFO      of clk   : signal is "xilinx.com:signal:clock:1.0 clk CLK";
   attribute X_INTERFACE_PARAMETER of clk   : signal is "ASSOCIATED_BUSIF m_axi:s_axi_imem:s_axi_dmem, ASSOCIATED_RESET reset:s_axi_aresetn";
   attribute X_INTERFACE_INFO      of reset : signal is "xilinx.com:signal:reset:1.0 reset RST";
   attribute X_INTERFACE_PARAMETER of reset : signal is "POLARITY ACTIVE_LOW";
   attribute X_INTERFACE_INFO      of s_axi_aresetn : signal is "xilinx.com:signal:reset:1.0 s_axi_aresetn RST";
   attribute X_INTERFACE_PARAMETER of s_axi_aresetn : signal is "POLARITY ACTIVE_LOW";

   signal instr_addr   : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal instr_rdata  : std_logic_vector(DATA_WIDTH-1  downto 0);

   -- Core data port
   signal req_s, ready_s, we_s, rsp_s : std_logic;
   signal addr_s, wdata_s, rdata_s    : std_logic_vector(31 downto 0);
   signal be_s                        : std_logic_vector(MEM_BYTES-1 downto 0);

   -- Data memory and AXI sides
   signal sel_dmem_s, dmem_req_s      : std_logic;
   signal dmem_we_s                   : std_logic_vector(3 downto 0);
   signal dmem_rdata_s                : std_logic_vector(31 downto 0);
   signal dmem_ready_s, dmem_rsp_s    : std_logic;
   signal axi_req_s, axi_ready_s      : std_logic;
   signal axi_rsp_s                   : std_logic;
   signal axi_rdata_s                 : std_logic_vector(31 downto 0);

   -- Port B of both memories
   signal imem_en_b_s, dmem_en_b_s    : std_logic;
   signal imem_we_b_s, dmem_we_b_s    : std_logic_vector(3 downto 0);
   signal imem_addr_b_s               : std_logic_vector(IMEM_ADDR_BITS-1 downto 0);
   signal dmem_addr_b_s               : std_logic_vector(DMEM_ADDR_BITS-1 downto 0);
   signal imem_din_b_s, dmem_din_b_s  : std_logic_vector(31 downto 0);
   signal imem_dout_b_s, dmem_dout_b_s : std_logic_vector(31 downto 0);

begin

   CPU: entity work.riscv_top
      generic map (
         DATA_WIDTH     => DATA_WIDTH,
         MEM_BYTES      => MEM_BYTES,
         ARCH_ADDR_BITS => ARCH_ADDR_BITS,
         NUM_PHYS_REGS  => NUM_PHYS_REGS,
         ROB_DEPTH      => ROB_DEPTH,
         IQ_DEPTH       => IQ_DEPTH,
         LSQ_DEPTH      => LSQ_DEPTH,
         RF_RAM_STYLE   => RF_RAM_STYLE,
         IQ_RAM_STYLE   => IQ_RAM_STYLE
      )
      port map (
         clk               => clk,
         reset             => reset,
         instr_mem_addr_o  => instr_addr,
         instr_mem_data_i  => instr_rdata,
         data_mem_req_o    => req_s,
         data_mem_ready_i  => ready_s,
         data_mem_we_o     => we_s,
         data_mem_addr_o   => addr_s,
         data_mem_wdata_o  => wdata_s,
         data_mem_be_o     => be_s,
         data_mem_rsp_i    => rsp_s,
         data_mem_rdata_i  => rdata_s
      );

   -- Instruction memory: read every cycle, handshake unused
   INSTR_MEM: entity work.BRAM
      generic map (
         ADDR_BITS => IMEM_ADDR_BITS,
         INIT_FILE => IMEM_INIT_FILE,
         SIM_LOAD  => SIM_LOAD
      )
      port map (
         clk        => clk,
         resetn_i   => '1',
         req_a_i    => '1',
         ready_a_o  => open,
         rsp_a_o    => open,
         en_b_i     => imem_en_b_s,
         data_a_i   => (others => '0'),
         data_b_i   => imem_din_b_s,
         addr_a_i   => instr_addr(IMEM_ADDR_BITS-1 downto 0),
         addr_b_i   => imem_addr_b_s,
         we_a_i     => "0000",
         we_b_i     => imem_we_b_s,
         data_a_o   => instr_rdata,
         data_b_o   => imem_dout_b_s
      );

   -- Port B of both memories for JTAG and DMA
   IMEM_AXI: entity work.axi_bram_ctrl
      generic map (ADDR_BITS => IMEM_ADDR_BITS, RD_LATENCY => 1)
      port map (
         clk           => clk,
         s_axi_aresetn => s_axi_aresetn,
         s_axi_awaddr  => s_axi_imem_awaddr,
         s_axi_awlen   => s_axi_imem_awlen,
         s_axi_awsize  => s_axi_imem_awsize,
         s_axi_awburst => s_axi_imem_awburst,
         s_axi_awvalid => s_axi_imem_awvalid,
         s_axi_awready => s_axi_imem_awready,
         s_axi_wdata   => s_axi_imem_wdata,
         s_axi_wstrb   => s_axi_imem_wstrb,
         s_axi_wlast   => s_axi_imem_wlast,
         s_axi_wvalid  => s_axi_imem_wvalid,
         s_axi_wready  => s_axi_imem_wready,
         s_axi_bresp   => s_axi_imem_bresp,
         s_axi_bvalid  => s_axi_imem_bvalid,
         s_axi_bready  => s_axi_imem_bready,
         s_axi_araddr  => s_axi_imem_araddr,
         s_axi_arlen   => s_axi_imem_arlen,
         s_axi_arsize  => s_axi_imem_arsize,
         s_axi_arburst => s_axi_imem_arburst,
         s_axi_arvalid => s_axi_imem_arvalid,
         s_axi_arready => s_axi_imem_arready,
         s_axi_rdata   => s_axi_imem_rdata,
         s_axi_rresp   => s_axi_imem_rresp,
         s_axi_rlast   => s_axi_imem_rlast,
         s_axi_rvalid  => s_axi_imem_rvalid,
         s_axi_rready  => s_axi_imem_rready,
         en_o          => imem_en_b_s,
         we_o          => imem_we_b_s,
         addr_o        => imem_addr_b_s,
         data_o        => imem_din_b_s,
         data_i        => imem_dout_b_s);

   DMEM_AXI: entity work.axi_bram_ctrl
      generic map (ADDR_BITS => DMEM_ADDR_BITS, RD_LATENCY => DMEM_RD_LATENCY)
      port map (
         clk           => clk,
         s_axi_aresetn => s_axi_aresetn,
         s_axi_awaddr  => s_axi_dmem_awaddr,
         s_axi_awlen   => s_axi_dmem_awlen,
         s_axi_awsize  => s_axi_dmem_awsize,
         s_axi_awburst => s_axi_dmem_awburst,
         s_axi_awvalid => s_axi_dmem_awvalid,
         s_axi_awready => s_axi_dmem_awready,
         s_axi_wdata   => s_axi_dmem_wdata,
         s_axi_wstrb   => s_axi_dmem_wstrb,
         s_axi_wlast   => s_axi_dmem_wlast,
         s_axi_wvalid  => s_axi_dmem_wvalid,
         s_axi_wready  => s_axi_dmem_wready,
         s_axi_bresp   => s_axi_dmem_bresp,
         s_axi_bvalid  => s_axi_dmem_bvalid,
         s_axi_bready  => s_axi_dmem_bready,
         s_axi_araddr  => s_axi_dmem_araddr,
         s_axi_arlen   => s_axi_dmem_arlen,
         s_axi_arsize  => s_axi_dmem_arsize,
         s_axi_arburst => s_axi_dmem_arburst,
         s_axi_arvalid => s_axi_dmem_arvalid,
         s_axi_arready => s_axi_dmem_arready,
         s_axi_rdata   => s_axi_dmem_rdata,
         s_axi_rresp   => s_axi_dmem_rresp,
         s_axi_rlast   => s_axi_dmem_rlast,
         s_axi_rvalid  => s_axi_dmem_rvalid,
         s_axi_rready  => s_axi_dmem_rready,
         en_o          => dmem_en_b_s,
         we_o          => dmem_we_b_s,
         addr_o        => dmem_addr_b_s,
         data_o        => dmem_din_b_s,
         data_i        => dmem_dout_b_s);

   -- Address decode: DMEM range or AXI, one request in flight
   sel_dmem_s <= '1' when unsigned(addr_s(31 downto DMEM_ADDR_BITS)) = to_unsigned(DMEM_BASE, 32)(31 downto DMEM_ADDR_BITS) else '0';
   dmem_req_s <= req_s and sel_dmem_s;
   axi_req_s  <= req_s and not sel_dmem_s;
   ready_s    <= dmem_ready_s when sel_dmem_s = '1' else axi_ready_s;
   dmem_we_s  <= be_s when we_s = '1' else "0000";

   DATA_MEM: entity work.BRAM
      generic map (
         ADDR_BITS  => DMEM_ADDR_BITS,
         RD_LATENCY => DMEM_RD_LATENCY,
         MIN_WAIT   => DATA_MEM_MIN_WAIT,
         MAX_WAIT   => DATA_MEM_MAX_WAIT,
         INIT_FILE  => DMEM_INIT_FILE,
         SIM_LOAD   => SIM_LOAD
      )
      port map (
         clk        => clk,
         resetn_i   => reset,
         req_a_i    => dmem_req_s,
         ready_a_o  => dmem_ready_s,
         rsp_a_o    => dmem_rsp_s,
         en_b_i     => dmem_en_b_s,
         data_a_i   => wdata_s,
         data_b_i   => dmem_din_b_s,
         addr_a_i   => addr_s(DMEM_ADDR_BITS-1 downto 0),
         addr_b_i   => dmem_addr_b_s,
         we_a_i     => dmem_we_s,
         we_b_i     => dmem_we_b_s,
         data_a_o   => dmem_rdata_s,
         data_b_o   => dmem_dout_b_s
      );

   rsp_s   <= dmem_rsp_s or axi_rsp_s;
   rdata_s <= dmem_rdata_s when dmem_rsp_s = '1' else axi_rdata_s;

   -- Rest of the address space on AXI4-Lite
   axi_bridge : entity work.axi_lite_master
      port map (
         clk           => clk,
         reset         => reset,
         req_valid_i   => axi_req_s,
         req_ready_o   => axi_ready_s,
         req_we_i      => we_s,
         req_addr_i    => addr_s,
         req_wdata_i   => wdata_s,
         req_be_i      => be_s,
         rsp_valid_o   => axi_rsp_s,
         rsp_rdata_o   => axi_rdata_s,
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

end rtl;

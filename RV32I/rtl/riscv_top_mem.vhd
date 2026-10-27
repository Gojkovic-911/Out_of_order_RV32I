library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Core with instruction memory
entity riscv_top_mem is
   generic
           (DATA_WIDTH     : positive := 32;
            MEM_BYTES      : positive := 4;
            ARCH_ADDR_BITS : positive := 5;
            NUM_PHYS_REGS  : positive := 64;
            ROB_DEPTH      : positive := 64;
            IQ_DEPTH       : positive := 16;
            LSQ_DEPTH      : positive := 16;
            WADDR          : natural  := 14;
            -- Program loaded at synthesis
            INIT_FILE   : string  := "");
   port(
      -- Clock and reset
      clk                 : in  std_logic;
      reset               : in  std_logic;

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
   attribute X_INTERFACE_PARAMETER of clk   : signal is "ASSOCIATED_BUSIF m_axi, ASSOCIATED_RESET reset";
   attribute X_INTERFACE_INFO      of reset : signal is "xilinx.com:signal:reset:1.0 reset RST";
   attribute X_INTERFACE_PARAMETER of reset : signal is "POLARITY ACTIVE_LOW";

   signal instr_addr   : std_logic_vector(DATA_WIDTH-1  downto 0);
   signal instr_rdata   : std_logic_vector(DATA_WIDTH-1  downto 0);

begin

   CPU: entity work.riscv_top
      generic map (
         DATA_WIDTH     => DATA_WIDTH,
         MEM_BYTES      => MEM_BYTES,
         ARCH_ADDR_BITS => ARCH_ADDR_BITS,
         NUM_PHYS_REGS  => NUM_PHYS_REGS,
         ROB_DEPTH      => ROB_DEPTH,
         IQ_DEPTH       => IQ_DEPTH,
         LSQ_DEPTH      => LSQ_DEPTH
      )
      port map (
         clk               => clk,
         reset             => reset,
         instr_mem_addr_o  => instr_addr,
         instr_mem_data_i  => instr_rdata,
         m_axi_awaddr      => m_axi_awaddr,
         m_axi_awprot      => m_axi_awprot,
         m_axi_awvalid     => m_axi_awvalid,
         m_axi_awready     => m_axi_awready,
         m_axi_wdata       => m_axi_wdata,
         m_axi_wstrb       => m_axi_wstrb,
         m_axi_wvalid      => m_axi_wvalid,
         m_axi_wready      => m_axi_wready,
         m_axi_bresp       => m_axi_bresp,
         m_axi_bvalid      => m_axi_bvalid,
         m_axi_bready      => m_axi_bready,
         m_axi_araddr      => m_axi_araddr,
         m_axi_arprot      => m_axi_arprot,
         m_axi_arvalid     => m_axi_arvalid,
         m_axi_arready     => m_axi_arready,
         m_axi_rdata       => m_axi_rdata,
         m_axi_rresp       => m_axi_rresp,
         m_axi_rvalid      => m_axi_rvalid,
         m_axi_rready      => m_axi_rready
      );

   -- Instruction memory from INIT_FILE
   INSTR_MEM: entity work.BRAM
      generic map (
         WADDR     => WADDR,
         INIT_FILE => INIT_FILE
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

end rtl;

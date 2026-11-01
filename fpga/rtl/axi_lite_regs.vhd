library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- AXI4-Lite slave for register blocks: word write strobe out, read data in
entity axi_lite_regs is
    generic (
        -- Byte address bits, 2^(ADDR_BITS-2) registers
        ADDR_BITS : natural := 4
    );
    port (
        s_axi_aclk    : in  std_logic;
        s_axi_aresetn : in  std_logic;
        s_axi_awaddr  : in  std_logic_vector(ADDR_BITS-1 downto 0);
        s_axi_awvalid : in  std_logic;
        s_axi_awready : out std_logic;
        s_axi_wdata   : in  std_logic_vector(31 downto 0);
        s_axi_wstrb   : in  std_logic_vector(3 downto 0);
        s_axi_wvalid  : in  std_logic;
        s_axi_wready  : out std_logic;
        s_axi_bresp   : out std_logic_vector(1 downto 0);
        s_axi_bvalid  : out std_logic;
        s_axi_bready  : in  std_logic;
        s_axi_araddr  : in  std_logic_vector(ADDR_BITS-1 downto 0);
        s_axi_arvalid : in  std_logic;
        s_axi_arready : out std_logic;
        s_axi_rdata   : out std_logic_vector(31 downto 0);
        s_axi_rresp   : out std_logic_vector(1 downto 0);
        s_axi_rvalid  : out std_logic;
        s_axi_rready  : in  std_logic;

        -- Register side: one-cycle write, read data sampled with rd_en
        wr_en   : out std_logic;
        wr_addr : out unsigned(ADDR_BITS-3 downto 0);
        wr_data : out std_logic_vector(31 downto 0);
        wr_strb : out std_logic_vector(3 downto 0);
        rd_en   : out std_logic;
        rd_addr : out unsigned(ADDR_BITS-3 downto 0);
        rd_data : in  std_logic_vector(31 downto 0)
    );
end entity;

architecture rtl of axi_lite_regs is
    signal write_s  : std_logic;
    signal read_s   : std_logic;
    signal bvalid_s : std_logic;
    signal rvalid_s : std_logic;
begin

    -- Write when address and data are both valid, read when no data is pending
    write_s       <= s_axi_awvalid and s_axi_wvalid and not bvalid_s;
    read_s        <= s_axi_arvalid and not rvalid_s;
    s_axi_awready <= write_s;
    s_axi_wready  <= write_s;
    s_axi_arready <= not rvalid_s;
    s_axi_bvalid  <= bvalid_s;
    s_axi_rvalid  <= rvalid_s;
    s_axi_bresp   <= "00";
    s_axi_rresp   <= "00";

    wr_en   <= write_s;
    wr_addr <= unsigned(s_axi_awaddr(ADDR_BITS-1 downto 2));
    wr_data <= s_axi_wdata;
    wr_strb <= s_axi_wstrb;
    rd_en   <= read_s;
    rd_addr <= unsigned(s_axi_araddr(ADDR_BITS-1 downto 2));

    resp: process(s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if read_s = '1' then
                s_axi_rdata <= rd_data;
            end if;
            if s_axi_aresetn = '0' then
                bvalid_s <= '0';
                rvalid_s <= '0';
            else
                if write_s = '1' then
                    bvalid_s <= '1';
                elsif s_axi_bready = '1' then
                    bvalid_s <= '0';
                end if;
                if read_s = '1' then
                    rvalid_s <= '1';
                elsif s_axi_rready = '1' then
                    rvalid_s <= '0';
                end if;
            end if;
        end if;
    end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Core data port (valid/ready request + response) to AXI4-Lite master
-- One transaction at a time: a request is accepted only in IDLE, the
-- response comes back as a one cycle pulse on rsp_valid_o.
--   write: AW and W sent together (accepted in any order), then B
--   read : AR, then R
-- AXI response codes are not checked.
entity axi_lite_master is
    generic (
        ADDR_WIDTH  : natural := 32;
        DATA_WIDTH  : natural := 32
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;    -- active low, as the core

        -- Core side
        req_valid_i     : in  std_logic;
        req_ready_o     : out std_logic;
        req_we_i        : in  std_logic;
        req_addr_i      : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        req_wdata_i     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        req_be_i        : in  std_logic_vector(DATA_WIDTH/8-1 downto 0);
        rsp_valid_o     : out std_logic;
        rsp_rdata_o     : out std_logic_vector(DATA_WIDTH-1 downto 0);

        -- AXI4-Lite master
        m_axi_awaddr    : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        m_axi_awprot    : out std_logic_vector(2 downto 0);
        m_axi_awvalid   : out std_logic;
        m_axi_awready   : in  std_logic;
        m_axi_wdata     : out std_logic_vector(DATA_WIDTH-1 downto 0);
        m_axi_wstrb     : out std_logic_vector(DATA_WIDTH/8-1 downto 0);
        m_axi_wvalid    : out std_logic;
        m_axi_wready    : in  std_logic;
        m_axi_bresp     : in  std_logic_vector(1 downto 0);
        m_axi_bvalid    : in  std_logic;
        m_axi_bready    : out std_logic;
        m_axi_araddr    : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        m_axi_arprot    : out std_logic_vector(2 downto 0);
        m_axi_arvalid   : out std_logic;
        m_axi_arready   : in  std_logic;
        m_axi_rdata     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        m_axi_rresp     : in  std_logic_vector(1 downto 0);
        m_axi_rvalid    : in  std_logic;
        m_axi_rready    : out std_logic
    );
end entity;

architecture Behavioral of axi_lite_master is

    type state_t is (IDLE, WRITE, WRITE_RESP, READ, READ_DATA);
    signal state_s      : state_t;

    signal addr_s       : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal wdata_s      : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal wstrb_s      : std_logic_vector(DATA_WIDTH/8-1 downto 0);
    signal awvalid_s    : std_logic;
    signal wvalid_s     : std_logic;
    signal arvalid_s    : std_logic;

begin

    req_ready_o     <= '1' when state_s = IDLE else '0';

    m_axi_awaddr    <= addr_s;
    m_axi_awprot    <= "000";
    m_axi_awvalid   <= awvalid_s;
    m_axi_wdata     <= wdata_s;
    m_axi_wstrb     <= wstrb_s;
    m_axi_wvalid    <= wvalid_s;
    m_axi_bready    <= '1' when state_s = WRITE_RESP else '0';
    m_axi_araddr    <= addr_s;
    m_axi_arprot    <= "000";
    m_axi_arvalid   <= arvalid_s;
    m_axi_rready    <= '1' when state_s = READ_DATA else '0';

    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '0' then
                state_s     <= IDLE;
                addr_s      <= (others => '0');
                wdata_s     <= (others => '0');
                wstrb_s     <= (others => '0');
                awvalid_s   <= '0';
                wvalid_s    <= '0';
                arvalid_s   <= '0';
                rsp_valid_o <= '0';
                rsp_rdata_o <= (others => '0');
            else
                rsp_valid_o <= '0';

                case state_s is
                    when IDLE =>
                        if (req_valid_i = '1') then
                            addr_s      <= req_addr_i;
                            wdata_s     <= req_wdata_i;
                            wstrb_s     <= req_be_i;
                            if (req_we_i = '1') then
                                awvalid_s   <= '1';
                                wvalid_s    <= '1';
                                state_s     <= WRITE;
                            else
                                arvalid_s   <= '1';
                                state_s     <= READ;
                            end if;
                        end if;

                    -- Address and data handshakes in any order
                    when WRITE =>
                        if (awvalid_s = '1' and m_axi_awready = '1') then
                            awvalid_s   <= '0';
                        end if;
                        if (wvalid_s = '1' and m_axi_wready = '1') then
                            wvalid_s    <= '0';
                        end if;
                        if ((awvalid_s = '0' or m_axi_awready = '1') and (wvalid_s = '0' or m_axi_wready = '1')) then
                            state_s     <= WRITE_RESP;
                        end if;

                    when WRITE_RESP =>
                        if (m_axi_bvalid = '1') then
                            rsp_valid_o <= '1';
                            state_s     <= IDLE;
                        end if;

                    when READ =>
                        if (m_axi_arready = '1') then
                            arvalid_s   <= '0';
                            state_s     <= READ_DATA;
                        end if;

                    when READ_DATA =>
                        if (m_axi_rvalid = '1') then
                            rsp_rdata_o <= m_axi_rdata;
                            rsp_valid_o <= '1';
                            state_s     <= IDLE;
                        end if;
                end case;
            end if;
        end if;
    end process;

end Behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- AXI4 slave on a BRAM port: INCR, WRAP and FIXED bursts up to 256 beats, no ID
entity axi_bram_ctrl is
    generic (
        -- Byte address bits of the memory
        ADDR_BITS  : natural  := 14;
        -- Read latency of the BRAM port: 1 or 2
        RD_LATENCY : positive := 1
    );
    port (
        clk           : in  std_logic;
        s_axi_aresetn : in  std_logic;

        s_axi_awaddr  : in  std_logic_vector(ADDR_BITS-1 downto 0);
        s_axi_awlen   : in  std_logic_vector(7 downto 0);
        s_axi_awsize  : in  std_logic_vector(2 downto 0);
        s_axi_awburst : in  std_logic_vector(1 downto 0);
        s_axi_awvalid : in  std_logic;
        s_axi_awready : out std_logic;
        s_axi_wdata   : in  std_logic_vector(31 downto 0);
        s_axi_wstrb   : in  std_logic_vector(3 downto 0);
        s_axi_wlast   : in  std_logic;
        s_axi_wvalid  : in  std_logic;
        s_axi_wready  : out std_logic;
        s_axi_bresp   : out std_logic_vector(1 downto 0);
        s_axi_bvalid  : out std_logic;
        s_axi_bready  : in  std_logic;
        s_axi_araddr  : in  std_logic_vector(ADDR_BITS-1 downto 0);
        s_axi_arlen   : in  std_logic_vector(7 downto 0);
        s_axi_arsize  : in  std_logic_vector(2 downto 0);
        s_axi_arburst : in  std_logic_vector(1 downto 0);
        s_axi_arvalid : in  std_logic;
        s_axi_arready : out std_logic;
        s_axi_rdata   : out std_logic_vector(31 downto 0);
        s_axi_rresp   : out std_logic_vector(1 downto 0);
        s_axi_rlast   : out std_logic;
        s_axi_rvalid  : out std_logic;
        s_axi_rready  : in  std_logic;

        -- BRAM port
        en_o          : out std_logic;
        we_o          : out std_logic_vector(3 downto 0);
        addr_o        : out std_logic_vector(ADDR_BITS-1 downto 0);
        data_o        : out std_logic_vector(31 downto 0);
        data_i        : in  std_logic_vector(31 downto 0)
    );
end entity;

architecture rtl of axi_bram_ctrl is
    type state_t is (S_IDLE, S_WRITE, S_WRESP, S_READ);
    -- Read data buffer: beats in flight in the BRAM plus one stall cycle
    constant DEPTH : natural := RD_LATENCY + 2;
    type fifo_t is array (0 to DEPTH - 1) of std_logic_vector(31 downto 0);

    signal state_s   : state_t;
    signal addr_s    : unsigned(ADDR_BITS-1 downto 0);
    signal len_s     : unsigned(7 downto 0);
    signal size_s    : unsigned(2 downto 0);
    signal burst_s   : std_logic_vector(1 downto 0);
    signal wbeat_s   : std_logic;

    signal issue_s   : std_logic;
    signal left_s    : unsigned(8 downto 0);
    signal pend_s    : natural range 0 to DEPTH;
    signal pipe_s    : std_logic_vector(RD_LATENCY-1 downto 0);
    signal fifo_s    : fifo_t;
    signal rd_ptr_s  : natural range 0 to DEPTH - 1;
    signal wr_ptr_s  : natural range 0 to DEPTH - 1;
    signal count_s   : natural range 0 to DEPTH;
    signal rbeat_s   : std_logic;
    signal sent_s    : unsigned(7 downto 0);

    function next_addr(a : unsigned; size : unsigned; len : unsigned; burst : std_logic_vector) return unsigned is
        variable inc  : unsigned(a'range) := shift_left(to_unsigned(1, a'length), to_integer(size));
        variable mask : unsigned(a'range);
    begin
        case burst is
            when "00" => return a;
            when "10" =>
                mask := resize(shift_left(resize(len, a'length) + 1, to_integer(size)) - 1, a'length);
                return (a and not mask) or ((a + inc) and mask);
            when others => return a + inc;
        end case;
    end function;
begin

    s_axi_awready <= '1' when state_s = S_IDLE else '0';
    s_axi_arready <= '1' when state_s = S_IDLE and s_axi_awvalid = '0' else '0';
    s_axi_wready  <= '1' when state_s = S_WRITE else '0';
    s_axi_bvalid  <= '1' when state_s = S_WRESP else '0';
    s_axi_bresp   <= "00";
    s_axi_rresp   <= "00";

    wbeat_s <= '1' when state_s = S_WRITE and s_axi_wvalid = '1' else '0';
    issue_s <= '1' when state_s = S_READ and left_s /= 0 and pend_s < DEPTH else '0';
    rbeat_s <= '1' when count_s /= 0 and s_axi_rready = '1' else '0';

    en_o   <= wbeat_s or issue_s;
    we_o   <= s_axi_wstrb when wbeat_s = '1' else "0000";
    addr_o <= std_logic_vector(addr_s);
    data_o <= s_axi_wdata;

    s_axi_rvalid <= '1' when count_s /= 0 else '0';
    s_axi_rdata  <= fifo_s(rd_ptr_s);
    s_axi_rlast  <= '1' when count_s /= 0 and sent_s = len_s else '0';

    -- Burst sequencer, writes before reads
    fsm: process(clk)
    begin
        if rising_edge(clk) then
            if s_axi_aresetn = '0' then
                state_s <= S_IDLE;
            else
                case state_s is
                    when S_IDLE =>
                        if s_axi_awvalid = '1' then
                            addr_s  <= unsigned(s_axi_awaddr);
                            len_s   <= unsigned(s_axi_awlen);
                            size_s  <= unsigned(s_axi_awsize);
                            burst_s <= s_axi_awburst;
                            state_s <= S_WRITE;
                        elsif s_axi_arvalid = '1' then
                            addr_s  <= unsigned(s_axi_araddr);
                            len_s   <= unsigned(s_axi_arlen);
                            size_s  <= unsigned(s_axi_arsize);
                            burst_s <= s_axi_arburst;
                            left_s  <= resize(unsigned(s_axi_arlen), 9) + 1;
                            sent_s  <= (others => '0');
                            state_s <= S_READ;
                        end if;
                    when S_WRITE =>
                        if wbeat_s = '1' then
                            addr_s <= next_addr(addr_s, size_s, len_s, burst_s);
                            if s_axi_wlast = '1' then
                                state_s <= S_WRESP;
                            end if;
                        end if;
                    when S_WRESP =>
                        if s_axi_bready = '1' then
                            state_s <= S_IDLE;
                        end if;
                    when S_READ =>
                        if issue_s = '1' then
                            addr_s <= next_addr(addr_s, size_s, len_s, burst_s);
                            left_s <= left_s - 1;
                        end if;
                        if rbeat_s = '1' then
                            sent_s <= sent_s + 1;
                            if sent_s = len_s then
                                state_s <= S_IDLE;
                            end if;
                        end if;
                end case;
            end if;
        end if;
    end process;

    -- Read data in flight, then buffered until rready
    rd_buf: process(clk)
        variable pend_v  : natural range 0 to DEPTH;
        variable count_v : natural range 0 to DEPTH;
    begin
        if rising_edge(clk) then
            if s_axi_aresetn = '0' then
                pipe_s   <= (others => '0');
                pend_s   <= 0;
                count_s  <= 0;
                rd_ptr_s <= 0;
                wr_ptr_s <= 0;
            else
                if RD_LATENCY = 1 then
                    pipe_s(0) <= issue_s;
                else
                    pipe_s <= pipe_s(RD_LATENCY-2 downto 0) & issue_s;
                end if;
                pend_v  := pend_s;
                count_v := count_s;
                if issue_s = '1' then
                    pend_v := pend_v + 1;
                end if;
                if pipe_s(RD_LATENCY-1) = '1' then
                    fifo_s(wr_ptr_s) <= data_i;
                    wr_ptr_s <= (wr_ptr_s + 1) mod DEPTH;
                    count_v  := count_v + 1;
                end if;
                if rbeat_s = '1' then
                    rd_ptr_s <= (rd_ptr_s + 1) mod DEPTH;
                    pend_v   := pend_v - 1;
                    count_v  := count_v - 1;
                end if;
                pend_s  <= pend_v;
                count_s <= count_v;
            end if;
        end if;
    end process;

end architecture;

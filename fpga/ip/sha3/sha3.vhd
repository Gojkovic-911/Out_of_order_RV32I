library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- SHA3-256 accelerator: AXI4-Lite registers, 64-bit AXI-Stream message in, digest out
-- 0x0 block count, 0x4 start (held until accepted), 0x8 status: bit0 ready, bit1 done
entity sha3 is
    port (
        aclk          : in  std_logic;
        aresetn       : in  std_logic;

        s_axi_awaddr  : in  std_logic_vector(3 downto 0);
        s_axi_awprot  : in  std_logic_vector(2 downto 0);
        s_axi_awvalid : in  std_logic;
        s_axi_awready : out std_logic;
        s_axi_wdata   : in  std_logic_vector(31 downto 0);
        s_axi_wstrb   : in  std_logic_vector(3 downto 0);
        s_axi_wvalid  : in  std_logic;
        s_axi_wready  : out std_logic;
        s_axi_bresp   : out std_logic_vector(1 downto 0);
        s_axi_bvalid  : out std_logic;
        s_axi_bready  : in  std_logic;
        s_axi_araddr  : in  std_logic_vector(3 downto 0);
        s_axi_arprot  : in  std_logic_vector(2 downto 0);
        s_axi_arvalid : in  std_logic;
        s_axi_arready : out std_logic;
        s_axi_rdata   : out std_logic_vector(31 downto 0);
        s_axi_rresp   : out std_logic_vector(1 downto 0);
        s_axi_rvalid  : out std_logic;
        s_axi_rready  : in  std_logic;

        -- Padded message, 17 lanes per block, little-endian
        s_axis_tdata  : in  std_logic_vector(63 downto 0);
        s_axis_tvalid : in  std_logic;
        s_axis_tready : out std_logic;
        s_axis_tlast  : in  std_logic;

        -- Digest, 4 lanes
        m_axis_tdata  : out std_logic_vector(63 downto 0);
        m_axis_tvalid : out std_logic;
        m_axis_tready : in  std_logic;
        m_axis_tlast  : out std_logic
    );
end entity;

architecture rtl of sha3 is
    signal rst_s     : std_logic;
    signal wr_en_s   : std_logic;
    signal wr_addr_s : unsigned(1 downto 0);
    signal wr_data_s : std_logic_vector(31 downto 0);
    signal rd_addr_s : unsigned(1 downto 0);
    signal rd_data_s : std_logic_vector(31 downto 0);

    signal nb_s, start_s, done_s, ready_s : std_logic_vector(31 downto 0);
    signal idle_s, accept_s, load_s, result_s : std_logic;
    signal clear_s, blk_clear_s, absorb_s, blk_full_s : std_logic;
    signal theta1_s, theta2_s, rhopi_s, chi_s, iota_s : std_logic;
    signal round_s   : unsigned(4 downto 0);

    signal in_ready_s, in_beat_s : std_logic;
    signal out_valid_s, out_beat_s, out_last_s : std_logic;
    signal out_cnt_s : unsigned(1 downto 0);
begin

    rst_s <= not aresetn;

    axi: entity work.axi_lite_regs
        generic map (ADDR_BITS => 4)
        port map (
            s_axi_aclk => aclk, s_axi_aresetn => aresetn,
            s_axi_awaddr => s_axi_awaddr, s_axi_awvalid => s_axi_awvalid, s_axi_awready => s_axi_awready,
            s_axi_wdata => s_axi_wdata, s_axi_wstrb => s_axi_wstrb, s_axi_wvalid => s_axi_wvalid,
            s_axi_wready => s_axi_wready, s_axi_bresp => s_axi_bresp, s_axi_bvalid => s_axi_bvalid,
            s_axi_bready => s_axi_bready, s_axi_araddr => s_axi_araddr, s_axi_arvalid => s_axi_arvalid,
            s_axi_arready => s_axi_arready, s_axi_rdata => s_axi_rdata, s_axi_rresp => s_axi_rresp,
            s_axi_rvalid => s_axi_rvalid, s_axi_rready => s_axi_rready,
            wr_en => wr_en_s, wr_addr => wr_addr_s, wr_data => wr_data_s, wr_strb => open,
            rd_en => open, rd_addr => rd_addr_s, rd_data => rd_data_s);

    -- Registers; done set by the last digest beat, cleared on start
    regs: process(aclk)
    begin
        if rising_edge(aclk) then
            if rst_s = '1' then
                nb_s     <= (others => '0');
                start_s  <= (others => '0');
                done_s   <= (others => '0');
            else
                if wr_en_s = '1' and wr_addr_s = 0 then
                    nb_s <= wr_data_s;
                end if;
                if wr_en_s = '1' and wr_addr_s = 1 then
                    start_s(0) <= wr_data_s(0);
                elsif accept_s = '1' then
                    start_s(0) <= '0';
                end if;
                if accept_s = '1' then
                    done_s(1) <= '0';
                elsif out_last_s = '1' then
                    done_s(1) <= '1';
                end if;
            end if;
        end if;
    end process;

    ready_s  <= (0 => idle_s and not out_valid_s, others => '0');
    with rd_addr_s select rd_data_s <=
        nb_s              when "00",
        start_s           when "01",
        done_s or ready_s when "10",
        (others => '0')   when others;

    control: entity work.sha3_control
        port map (
            clk => aclk, rst => rst_s, start => start_s(0), nb => nb_s, out_busy => out_valid_s,
            blk_full => blk_full_s, idle => idle_s, accept => accept_s, load => load_s,
            result => result_s, clear => clear_s, blk_clear => blk_clear_s, absorb => absorb_s,
            theta1 => theta1_s, theta2 => theta2_s, rhopi => rhopi_s, chi => chi_s, iota => iota_s,
            round => round_s);

    datapath: entity work.sha3_datapath
        port map (
            clk => aclk, rst => rst_s, clear => clear_s, absorb => absorb_s,
            theta1 => theta1_s, theta2 => theta2_s, rhopi => rhopi_s, chi => chi_s, iota => iota_s,
            round => round_s, blk_clear => blk_clear_s, in_valid => in_beat_s, in_data => s_axis_tdata,
            blk_full => blk_full_s, out_sel => out_cnt_s, out_data => m_axis_tdata);

    -- Input beats only while waiting for a block
    in_ready_s    <= load_s and not blk_full_s;
    in_beat_s     <= s_axis_tvalid and in_ready_s;
    s_axis_tready <= in_ready_s;

    -- Digest lanes from result until the 4th handshake
    out_beat_s    <= out_valid_s and m_axis_tready;
    out_last_s    <= '1' when out_beat_s = '1' and out_cnt_s = 3 else '0';
    m_axis_tvalid <= out_valid_s;
    m_axis_tlast  <= '1' when out_valid_s = '1' and out_cnt_s = 3 else '0';

    stream_out: process(aclk)
    begin
        if rising_edge(aclk) then
            if rst_s = '1' or result_s = '1' then
                out_cnt_s <= (others => '0');
            elsif out_beat_s = '1' then
                out_cnt_s <= out_cnt_s + 1;
            end if;
            if rst_s = '1' or out_last_s = '1' then
                out_valid_s <= '0';
            elsif result_s = '1' then
                out_valid_s <= '1';
            end if;
        end if;
    end process;

end architecture;

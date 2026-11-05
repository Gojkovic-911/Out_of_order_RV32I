library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- AXI4 slave on BRAM port B: random bursts, strobes and stalls against a reference memory
entity axi_bram_ctrl_tb is
    generic (
        RD_LATENCY : positive := 1;
        NUM_TRANS  : positive := 2000);
end entity;

architecture tb of axi_bram_ctrl_tb is
    constant ADDR_BITS : natural := 10;
    constant WORDS     : natural := 2**(ADDR_BITS - 2);
    type mem_t is array (0 to WORDS - 1) of std_logic_vector(31 downto 0);

    signal clk     : std_logic := '0';
    signal aresetn : std_logic := '0';

    signal awaddr, araddr   : std_logic_vector(ADDR_BITS-1 downto 0) := (others => '0');
    signal awlen, arlen     : std_logic_vector(7 downto 0) := (others => '0');
    signal awsize, arsize   : std_logic_vector(2 downto 0) := "010";
    signal awburst, arburst : std_logic_vector(1 downto 0) := "01";
    signal awvalid, awready, wvalid, wready, wlast, bvalid, bready : std_logic := '0';
    signal arvalid, arready, rvalid, rready, rlast : std_logic := '0';
    signal wdata, rdata     : std_logic_vector(31 downto 0) := (others => '0');
    signal wstrb            : std_logic_vector(3 downto 0) := "0000";
    signal bresp, rresp     : std_logic_vector(1 downto 0);

    signal en_b    : std_logic;
    signal we_b    : std_logic_vector(3 downto 0);
    signal addr_b  : std_logic_vector(ADDR_BITS-1 downto 0);
    signal din_b, dout_b : std_logic_vector(31 downto 0);

    signal errors  : natural := 0;
begin
    clk <= not clk after 5 ns;

    mem: entity work.BRAM
        generic map (ADDR_BITS => ADDR_BITS, RD_LATENCY => RD_LATENCY)
        port map (clk => clk, resetn_i => aresetn, req_a_i => '0', ready_a_o => open, rsp_a_o => open, en_b_i => en_b, data_a_i => (others => '0'),
                  data_b_i => din_b, addr_a_i => (others => '0'), addr_b_i => addr_b,
                  we_a_i => "0000", we_b_i => we_b, data_a_o => open, data_b_o => dout_b);

    dut: entity work.axi_bram_ctrl
        generic map (ADDR_BITS => ADDR_BITS, RD_LATENCY => RD_LATENCY)
        port map (
            clk => clk, s_axi_aresetn => aresetn,
            s_axi_awaddr => awaddr, s_axi_awlen => awlen, s_axi_awsize => awsize, s_axi_awburst => awburst,
            s_axi_awvalid => awvalid, s_axi_awready => awready, s_axi_wdata => wdata, s_axi_wstrb => wstrb,
            s_axi_wlast => wlast, s_axi_wvalid => wvalid, s_axi_wready => wready, s_axi_bresp => bresp,
            s_axi_bvalid => bvalid, s_axi_bready => bready, s_axi_araddr => araddr, s_axi_arlen => arlen,
            s_axi_arsize => arsize, s_axi_arburst => arburst, s_axi_arvalid => arvalid, s_axi_arready => arready,
            s_axi_rdata => rdata, s_axi_rresp => rresp, s_axi_rlast => rlast, s_axi_rvalid => rvalid,
            s_axi_rready => rready,
            en_o => en_b, we_o => we_b, addr_o => addr_b, data_o => din_b, data_i => dout_b);

    -- Random rready and bready
    ready_gen: process(clk)
        variable s1 : positive := 3;
        variable s2 : positive := 5;
        variable r  : real;
    begin
        if rising_edge(clk) then
            uniform(s1, s2, r);
            rready <= '1' when r < 0.7 else '0';
            uniform(s1, s2, r);
            bready <= '1' when r < 0.5 else '0';
        end if;
    end process;

    master: process
        variable ref    : mem_t := (others => (others => '0'));
        variable s1     : positive := 11;
        variable s2     : positive := 13;
        variable r      : real;
        variable start  : natural;
        variable len    : natural;
        variable burst  : std_logic_vector(1 downto 0);
        variable total  : natural;
        variable base   : natural;
        variable a, w   : natural;
        variable data   : std_logic_vector(31 downto 0);
        variable strb   : std_logic_vector(3 downto 0);
        variable last   : std_logic;
        variable writes, reads, beats : natural := 0;

        impure function rnd(n : positive) return natural is
        begin
            uniform(s1, s2, r);
            return integer(trunc(r * real(n)));
        end function;

        -- Byte address of beat i, computed apart from the RTL
        function beat_addr(start, i, len : natural; burst : std_logic_vector) return natural is
            variable tot, b : natural;
        begin
            case burst is
                when "00" => return start;
                when "10" =>
                    tot := (len + 1) * 4;
                    b   := start - (start mod tot);
                    return b + ((start - b + 4 * i) mod tot);
                when others => return (start + 4 * i) mod 2**ADDR_BITS;
            end case;
        end function;

        procedure gap is
        begin
            for k in 1 to rnd(3) loop wait until rising_edge(clk); end loop;
        end procedure;
    begin
        for i in 1 to 5 loop wait until rising_edge(clk); end loop;
        aresetn <= '1';
        wait until rising_edge(clk);

        for t in 1 to NUM_TRANS loop
            -- Burst type and length: INCR any length, WRAP 2/4/8/16, FIXED
            case rnd(4) is
                when 0 => burst := "10"; len := 2**(1 + rnd(4)) - 1;
                when 1 => burst := "00"; len := rnd(8);
                when others =>
                    burst := "01";
                    len   := rnd(16) when rnd(10) /= 0 else rnd(256);
            end case;
            start := 4 * rnd(WORDS);
            gap;
            if rnd(2) = 0 then
                awaddr <= std_logic_vector(to_unsigned(start, ADDR_BITS));
                awlen <= std_logic_vector(to_unsigned(len, 8)); awburst <= burst;
                awvalid <= '1';
                for i in 0 to len loop
                    data := std_logic_vector(to_unsigned(rnd(65536), 16)) & std_logic_vector(to_unsigned(rnd(65536), 16));
                    strb := std_logic_vector(to_unsigned(rnd(16), 4));
                    wdata <= data; wstrb <= strb; wvalid <= '1';
                    wlast <= '1' when i = len else '0';
                    loop
                        wait until rising_edge(clk);
                        if awready = '1' then awvalid <= '0'; end if;
                        exit when wready = '1';
                    end loop;
                    w := beat_addr(start, i, len, burst) / 4;
                    for b in 0 to 3 loop
                        if strb(b) = '1' then ref(w)(8*b + 7 downto 8*b) := data(8*b + 7 downto 8*b); end if;
                    end loop;
                    wvalid <= '0'; wlast <= '0';
                    if rnd(3) = 0 and i /= len then gap; end if;
                end loop;
                loop
                    wait until rising_edge(clk);
                    exit when bvalid = '1' and bready = '1';
                end loop;
                writes := writes + 1;
            else
                araddr <= std_logic_vector(to_unsigned(start, ADDR_BITS));
                arlen <= std_logic_vector(to_unsigned(len, 8)); arburst <= burst;
                arvalid <= '1';
                loop
                    wait until rising_edge(clk);
                    exit when arready = '1';
                end loop;
                arvalid <= '0';
                for i in 0 to len loop
                    loop
                        wait until rising_edge(clk);
                        exit when rvalid = '1' and rready = '1';
                    end loop;
                    w := beat_addr(start, i, len, burst) / 4;
                    last := '1' when i = len else '0';
                    if rdata /= ref(w) or rlast /= last then
                        report "read " & integer'image(t) & " beat " & integer'image(i) & " word " & integer'image(w) &
                               ": got " & to_hstring(rdata) & " rlast " & std_logic'image(rlast) &
                               ", expected " & to_hstring(ref(w)) severity error;
                        errors <= errors + 1;
                    end if;
                end loop;
                reads := reads + 1;
            end if;
            beats := beats + len + 1;
        end loop;

        wait until rising_edge(clk);
        if errors = 0 then
            report "AXI BRAM PASS: RD_LATENCY " & integer'image(RD_LATENCY) & ", " & integer'image(writes) &
                   " writes, " & integer'image(reads) & " reads, " & integer'image(beats) & " beats";
        else
            report "AXI BRAM FAIL: " & integer'image(errors) & " errors" severity error;
        end if;
        std.env.finish;
    end process;

    watchdog: process
    begin
        wait for 50 ms;
        report "AXI BRAM FAIL: timeout" severity failure;
    end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;

-- SHA3 IP: AXI-Lite control, AXIS in/out with random stalls
entity sha3_ip_tb is
    generic (
        VEC_DIR : string := "out/tb/fpga/ip/sha3");
end entity;

architecture tb of sha3_ip_tb is
    constant CLK_PERIOD : time := 10 ns;
    constant REG_BLOCKS : std_logic_vector(3 downto 0) := "0000";
    constant REG_START  : std_logic_vector(3 downto 0) := "0100";
    constant REG_STATUS : std_logic_vector(3 downto 0) := "1000";

    signal clk     : std_logic := '0';
    signal aresetn : std_logic := '0';

    signal awaddr, araddr  : std_logic_vector(3 downto 0) := (others => '0');
    signal awvalid, awready, wvalid, wready, bvalid, bready : std_logic := '0';
    signal arvalid, arready, rvalid, rready : std_logic := '0';
    signal wdata, rdata : std_logic_vector(31 downto 0) := (others => '0');
    signal bresp, rresp : std_logic_vector(1 downto 0);

    signal s_tdata, m_tdata : std_logic_vector(63 downto 0) := (others => '0');
    signal s_tvalid, s_tready, s_tlast : std_logic := '0';
    signal m_tvalid, m_tready, m_tlast : std_logic := '0';

    signal msgs_checked : natural := 0;
    signal errors       : natural := 0;
    signal num_msgs     : natural := 0;
begin
    clk <= not clk after CLK_PERIOD / 2;

    dut: entity work.sha3
        port map (
            aclk => clk, aresetn => aresetn,
            s_axi_awaddr => awaddr, s_axi_awprot => "000", s_axi_awvalid => awvalid,
            s_axi_awready => awready, s_axi_wdata => wdata, s_axi_wstrb => "1111",
            s_axi_wvalid => wvalid, s_axi_wready => wready, s_axi_bresp => bresp,
            s_axi_bvalid => bvalid, s_axi_bready => bready,
            s_axi_araddr => araddr, s_axi_arprot => "000", s_axi_arvalid => arvalid,
            s_axi_arready => arready, s_axi_rdata => rdata, s_axi_rresp => rresp,
            s_axi_rvalid => rvalid, s_axi_rready => rready,
            s_axis_tdata => s_tdata, s_axis_tvalid => s_tvalid, s_axis_tready => s_tready,
            s_axis_tlast => s_tlast,
            m_axis_tdata => m_tdata, m_axis_tvalid => m_tvalid, m_axis_tready => m_tready,
            m_axis_tlast => m_tlast);

    -- Control: block count, start, poll status
    control: process
        file f       : text;
        variable l   : line;
        variable nb  : integer;
        variable w   : std_logic_vector(63 downto 0);
        variable rd  : std_logic_vector(31 downto 0);
        variable n   : natural := 0;

        procedure axi_write(addr : std_logic_vector(3 downto 0); data : natural) is
        begin
            awaddr <= addr; wdata <= std_logic_vector(to_unsigned(data, 32));
            awvalid <= '1'; wvalid <= '1'; bready <= '1';
            loop
                wait until rising_edge(clk);
                exit when awready = '1' and wready = '1';
            end loop;
            awvalid <= '0'; wvalid <= '0';
            while bvalid /= '1' loop
                wait until rising_edge(clk);
            end loop;
            wait until rising_edge(clk);
            bready <= '0';
        end procedure;

        procedure axi_read(addr : std_logic_vector(3 downto 0); data : out std_logic_vector(31 downto 0)) is
        begin
            araddr <= addr; arvalid <= '1'; rready <= '1';
            loop
                wait until rising_edge(clk);
                exit when arready = '1';
            end loop;
            arvalid <= '0';
            while rvalid /= '1' loop
                wait until rising_edge(clk);
            end loop;
            data := rdata;
            wait until rising_edge(clk);
            rready <= '0';
        end procedure;
    begin
        aresetn <= '0';
        for i in 1 to 10 loop wait until rising_edge(clk); end loop;
        aresetn <= '1';
        wait until rising_edge(clk);

        file_open(f, VEC_DIR & "/in.txt", read_mode);
        while not endfile(f) loop
            readline(f, l); read(l, nb);
            for i in 1 to 17 * nb loop readline(f, l); end loop;

            loop
                axi_read(REG_STATUS, rd);
                exit when rd(0) = '1';
            end loop;
            axi_write(REG_BLOCKS, nb);
            axi_write(REG_START, 1);
            loop
                axi_read(REG_STATUS, rd);
                exit when rd(1) = '1';
            end loop;
            n := n + 1;
        end loop;
        file_close(f);
        num_msgs <= n;

        wait until msgs_checked = n for 10 us;
        if errors = 0 and msgs_checked = n then
            report "SHA3 PASS: " & integer'image(n) & " messages";
        else
            report "SHA3 FAIL: " & integer'image(errors) & " errors, " &
                   integer'image(msgs_checked) & "/" & integer'image(n) & " checked" severity error;
        end if;
        std.env.finish;
    end process;

    -- DMA MM2S model: all messages back to back, random gaps
    stream_in: process
        file f       : text;
        variable l   : line;
        variable nb  : integer;
        variable w   : std_logic_vector(63 downto 0);
        variable s1  : positive := 7;
        variable s2  : positive := 11;
        variable r   : real;
    begin
        wait until aresetn = '1';
        file_open(f, VEC_DIR & "/in.txt", read_mode);
        while not endfile(f) loop
            readline(f, l); read(l, nb);
            for i in 1 to 17 * nb loop
                readline(f, l); hread(l, w);
                uniform(s1, s2, r);
                if r < 0.3 then
                    s_tvalid <= '0';
                    for k in 0 to integer(r * 10.0) loop wait until rising_edge(clk); end loop;
                end if;
                s_tdata <= w; s_tvalid <= '1';
                s_tlast <= '1' when i = 17 * nb else '0';
                loop
                    wait until rising_edge(clk);
                    exit when s_tready = '1';
                end loop;
            end loop;
        end loop;
        s_tvalid <= '0'; s_tlast <= '0';
        file_close(f);
        wait;
    end process;

    -- Random tready
    ready_gen: process(clk)
        variable s1 : positive := 3;
        variable s2 : positive := 5;
        variable r  : real;
    begin
        if rising_edge(clk) then
            uniform(s1, s2, r);
            m_tready <= '1' when r < 0.6 else '0';
        end if;
    end process;

    -- DMA S2MM model: 4 lanes per digest
    check: process(clk)
        file f       : text open read_mode is VEC_DIR & "/exp.txt";
        variable l   : line;
        variable exp : std_logic_vector(63 downto 0);
        variable beat : natural := 0;
        variable last : std_logic;
    begin
        if rising_edge(clk) then
            if m_tvalid = '1' and m_tready = '1' then
                readline(f, l); hread(l, exp);
                last := '1' when beat = 3 else '0';
                if m_tdata /= exp or m_tlast /= last then
                    report "msg " & integer'image(msgs_checked) & " lane " & integer'image(beat) &
                           ": got " & to_hstring(m_tdata) & " tlast " & std_logic'image(m_tlast) &
                           ", expected " & to_hstring(exp) severity error;
                    errors <= errors + 1;
                end if;
                if beat = 3 then
                    beat := 0;
                    msgs_checked <= msgs_checked + 1;
                else
                    beat := beat + 1;
                end if;
            end if;
        end if;
    end process;

    -- tvalid must stay up until the handshake
    axis_rule: process(clk)
        variable prev_valid, prev_ready : std_logic := '0';
    begin
        if rising_edge(clk) then
            if prev_valid = '1' and prev_ready = '0' and m_tvalid = '0' then
                report "m_axis_tvalid dropped without handshake" severity error;
            end if;
            prev_valid := m_tvalid;
            prev_ready := m_tready;
        end if;
    end process;

    watchdog: process
    begin
        wait for 5 ms;
        report "SHA3 FAIL: timeout, " & integer'image(msgs_checked) & " checked" severity failure;
    end process;
end architecture;

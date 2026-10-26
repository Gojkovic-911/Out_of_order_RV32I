library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- AXI4-Lite memory, random waits
entity axi_lite_mem_model is
    generic (
        WADDR       : natural := 14;
        MAX_WAIT    : natural := 3;
        SEED        : natural := 16#ACE1#
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;

        s_axi_awaddr    : in  std_logic_vector(31 downto 0);
        s_axi_awvalid   : in  std_logic;
        s_axi_awready   : out std_logic;
        s_axi_wdata     : in  std_logic_vector(31 downto 0);
        s_axi_wstrb     : in  std_logic_vector(3 downto 0);
        s_axi_wvalid    : in  std_logic;
        s_axi_wready    : out std_logic;
        s_axi_bresp     : out std_logic_vector(1 downto 0);
        s_axi_bvalid    : out std_logic;
        s_axi_bready    : in  std_logic;
        s_axi_araddr    : in  std_logic_vector(31 downto 0);
        s_axi_arvalid   : in  std_logic;
        s_axi_arready   : out std_logic;
        s_axi_rdata     : out std_logic_vector(31 downto 0);
        s_axi_rresp     : out std_logic_vector(1 downto 0);
        s_axi_rvalid    : out std_logic;
        s_axi_rready    : in  std_logic
    );
end entity;

architecture sim of axi_lite_mem_model is

    type mem_t is array (0 to 2**WADDR - 1) of std_logic_vector(7 downto 0);
    signal mem_s : mem_t := (others => (others => '0'));
    
    -- Internal copies of outputs
    signal awready_s, wready_s, bvalid_s, arready_s, rvalid_s : std_logic;

begin

    s_axi_bresp   <= "00";
    s_axi_rresp   <= "00";
    s_axi_awready <= awready_s;
    s_axi_wready  <= wready_s;
    s_axi_bvalid  <= bvalid_s;
    s_axi_arready <= arready_s;
    s_axi_rvalid  <= rvalid_s;

    process(clk)
        variable lfsr       : unsigned(15 downto 0) := to_unsigned(SEED, 16);
        variable aw_wait    : natural := 0;
        variable w_wait     : natural := 0;
        variable b_wait     : natural := 0;
        variable ar_wait    : natural := 0;
        variable r_wait     : natural := 0;
        variable aw_have    : boolean := false;
        variable w_have     : boolean := false;
        variable b_pending  : boolean := false;
        variable r_pending  : boolean := false;
        variable awaddr     : unsigned(WADDR-1 downto 0);
        variable wdata      : std_logic_vector(31 downto 0);
        variable wstrb      : std_logic_vector(3 downto 0);
        variable a          : natural;

        impure function next_wait return natural is
        begin
            -- 16-bit LFSR wait generator
            lfsr := lfsr(14 downto 0) & (lfsr(15) xor lfsr(13) xor lfsr(12) xor lfsr(10));
            if MAX_WAIT = 0 then
                return 0;
            end if;
            return to_integer(lfsr(7 downto 0)) mod (MAX_WAIT + 1);
        end function;
    begin
        if rising_edge(clk) then
            if reset = '0' then
                awready_s <= '0';
                wready_s  <= '0';
                bvalid_s  <= '0';
                arready_s <= '0';
                rvalid_s  <= '0';
                s_axi_rdata   <= (others => '0');
                aw_have   := false;
                w_have    := false;
                b_pending := false;
                r_pending := false;
                aw_wait   := next_wait;
                w_wait    := next_wait;
                ar_wait   := next_wait;
            else
                -- Write address channel
                awready_s <= '0';
                if (s_axi_awvalid = '1' and not aw_have and not b_pending) then
                    if aw_wait = 0 then
                        awready_s <= '1';
                    else
                        aw_wait := aw_wait - 1;
                    end if;
                end if;
                if (s_axi_awvalid = '1' and awready_s = '1') then
                    awready_s <= '0';
                    awaddr  := unsigned(s_axi_awaddr(WADDR-1 downto 0));
                    aw_have := true;
                    aw_wait := next_wait;
                end if;

                -- Write data channel
                wready_s <= '0';
                if (s_axi_wvalid = '1' and not w_have and not b_pending) then
                    if w_wait = 0 then
                        wready_s <= '1';
                    else
                        w_wait := w_wait - 1;
                    end if;
                end if;
                if (s_axi_wvalid = '1' and wready_s = '1') then
                    wready_s <= '0';
                    wdata  := s_axi_wdata;
                    wstrb  := s_axi_wstrb;
                    w_have := true;
                    w_wait := next_wait;
                end if;

                -- Memory write and response
                if (aw_have and w_have) then
                    a := to_integer(awaddr(WADDR-1 downto 2)) * 4;
                    for i in 0 to 3 loop
                        if wstrb(i) = '1' then
                            mem_s(a + i) <= wdata(8*i+7 downto 8*i);
                        end if;
                    end loop;
                    aw_have   := false;
                    w_have    := false;
                    b_pending := true;
                    b_wait    := next_wait;
                end if;
                if b_pending then
                    if (bvalid_s = '1' and s_axi_bready = '1') then
                        bvalid_s <= '0';
                        b_pending    := false;
                    elsif b_wait = 0 then
                        bvalid_s <= '1';
                    else
                        b_wait := b_wait - 1;
                    end if;
                end if;

                -- Read channel
                arready_s <= '0';
                if (s_axi_arvalid = '1' and not r_pending) then
                    if ar_wait = 0 then
                        arready_s <= '1';
                    else
                        ar_wait := ar_wait - 1;
                    end if;
                end if;
                if (s_axi_arvalid = '1' and arready_s = '1') then
                    arready_s <= '0';
                    a := to_integer(unsigned(s_axi_araddr(WADDR-1 downto 2))) * 4;
                    s_axi_rdata <= mem_s(a + 3) & mem_s(a + 2) & mem_s(a + 1) & mem_s(a);
                    r_pending := true;
                    r_wait    := next_wait;
                    ar_wait   := next_wait;
                end if;
                if r_pending then
                    if (rvalid_s = '1' and s_axi_rready = '1') then
                        rvalid_s <= '0';
                        r_pending    := false;
                    elsif r_wait = 0 then
                        rvalid_s <= '1';
                    else
                        r_wait := r_wait - 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

end sim;

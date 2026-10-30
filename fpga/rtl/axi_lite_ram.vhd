library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

-- AXI4-Lite block RAM, contents from INIT_FILE
entity axi_lite_ram is
    generic (
        -- Byte address bits, 2^ADDR_BITS bytes
        ADDR_BITS : natural := 14;
        -- One 32-bit binary word per line, empty: all zeros
        INIT_FILE : string  := "";
        -- Simulation: reload INIT_FILE on the first clock (restart, no new elaboration)
        SIM_LOAD  : boolean := false
    );
    port (
        s_axi_aclk    : in  std_logic;
        s_axi_aresetn : in  std_logic;
        s_axi_awaddr  : in  std_logic_vector(ADDR_BITS-1 downto 0);
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
        s_axi_araddr  : in  std_logic_vector(ADDR_BITS-1 downto 0);
        s_axi_arprot  : in  std_logic_vector(2 downto 0);
        s_axi_arvalid : in  std_logic;
        s_axi_arready : out std_logic;
        s_axi_rdata   : out std_logic_vector(31 downto 0);
        s_axi_rresp   : out std_logic_vector(1 downto 0);
        s_axi_rvalid  : out std_logic;
        s_axi_rready  : in  std_logic
    );
end entity;

architecture rtl of axi_lite_ram is
    constant WORDS : natural := 2**(ADDR_BITS - 2);
    type ram_type is array(0 to WORDS - 1) of std_logic_vector(31 downto 0);

    impure function read_init_file(name : string) return ram_type is
        file     f    : text;
        variable st   : file_open_status;
        variable row  : line;
        variable word : bit_vector(31 downto 0);
        variable ram  : ram_type := (others => (others => '0'));
        variable a    : natural  := 0;
    begin
        if name = "" then
            return ram;
        end if;
        file_open(st, f, name, read_mode);
        if st /= open_ok then
            return ram;
        end if;
        while (not endfile(f)) and (a < WORDS) loop
            readline(f, row);
            if row'length > 0 then
                read(row, word);
                ram(a) := to_stdlogicvector(word);
                a := a + 1;
            end if;
        end loop;
        file_close(f);
        return ram;
    end function;

    signal ram_s    : ram_type := read_init_file(INIT_FILE);
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

    -- Byte writes, registered read
    mem: process(s_axi_aclk)
        variable wi : natural range 0 to WORDS - 1;
        variable ri : natural range 0 to WORDS - 1;
        -- pragma translate_off
        variable loaded : boolean := false;
        -- pragma translate_on
    begin
        if rising_edge(s_axi_aclk) then
            -- pragma translate_off
            if SIM_LOAD and not loaded then
                loaded := true;
                ram_s  <= read_init_file(INIT_FILE);
            end if;
            -- pragma translate_on
            wi := to_integer(unsigned(s_axi_awaddr(ADDR_BITS-1 downto 2)));
            ri := to_integer(unsigned(s_axi_araddr(ADDR_BITS-1 downto 2)));
            if write_s = '1' then
                for i in 0 to 3 loop
                    if s_axi_wstrb(i) = '1' then
                        ram_s(wi)(8*i + 7 downto 8*i) <= s_axi_wdata(8*i + 7 downto 8*i);
                    end if;
                end loop;
            end if;
            if read_s = '1' then
                s_axi_rdata <= ram_s(ri);
            end if;
        end if;
    end process;

    -- Response handshakes
    resp: process(s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
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

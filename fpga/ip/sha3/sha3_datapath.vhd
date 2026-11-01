library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Keccak-f[1600] state, one step per cycle, and the 17-lane input block
entity sha3_datapath is
    port (
        clk       : in  std_logic;
        rst       : in  std_logic;

        -- Steps from sha3_control, at most one per cycle
        clear     : in  std_logic;
        absorb    : in  std_logic;
        theta1    : in  std_logic;
        theta2    : in  std_logic;
        rhopi     : in  std_logic;
        chi       : in  std_logic;
        iota      : in  std_logic;
        round     : in  unsigned(4 downto 0);

        -- Input block, 17 lanes in order
        blk_clear : in  std_logic;
        in_valid  : in  std_logic;
        in_data   : in  std_logic_vector(63 downto 0);
        blk_full  : out std_logic;

        -- Digest lane A(sel, 0)
        out_sel   : in  unsigned(1 downto 0);
        out_data  : out std_logic_vector(63 downto 0)
    );
end entity;

architecture rtl of sha3_datapath is
    subtype lane_t is std_logic_vector(63 downto 0);
    type state_t is array (0 to 4, 0 to 4) of lane_t;
    type row_t   is array (0 to 4) of lane_t;
    type block_t is array (0 to 16) of lane_t;
    type rho_t   is array (0 to 4, 0 to 4) of natural;
    type rc_t    is array (0 to 23) of lane_t;

    -- Rotation offsets r(x, y)
    constant RHO : rho_t := (
        ( 0, 36,  3, 41, 18),
        ( 1, 44, 10, 45,  2),
        (62,  6, 43, 15, 61),
        (28, 55, 25, 21, 56),
        (27, 20, 39,  8, 14));

    constant RC : rc_t := (
        x"0000000000000001", x"0000000000008082", x"800000000000808a", x"8000000080008000",
        x"000000000000808b", x"0000000080000001", x"8000000080008081", x"8000000000008009",
        x"000000000000008a", x"0000000000000088", x"0000000080008009", x"000000008000000a",
        x"000000008000808b", x"800000000000008b", x"8000000000008089", x"8000000000008003",
        x"8000000000008002", x"8000000000000080", x"000000000000800a", x"800000008000000a",
        x"8000000080008081", x"8000000000008080", x"0000000080000001", x"8000000080008008");

    function rotl(v : lane_t; n : natural) return lane_t is
    begin
        return std_logic_vector(rotate_left(unsigned(v), n));
    end function;

    signal a_s    : state_t;
    signal b_s    : state_t;
    signal c_s    : row_t;
    signal blk_s  : block_t;
    signal fill_s : unsigned(4 downto 0);
begin

    -- State A(x, y), B after rho and pi, column parity C
    keccak: process(clk)
    begin
        if rising_edge(clk) then
            if clear = '1' then
                a_s <= (others => (others => (others => '0')));
            elsif absorb = '1' then
                for w in 0 to 16 loop
                    a_s(w mod 5, w / 5) <= a_s(w mod 5, w / 5) xor blk_s(w);
                end loop;
            elsif theta2 = '1' then
                for x in 0 to 4 loop
                    for y in 0 to 4 loop
                        a_s(x, y) <= a_s(x, y) xor c_s((x + 4) mod 5) xor rotl(c_s((x + 1) mod 5), 1);
                    end loop;
                end loop;
            elsif chi = '1' then
                for x in 0 to 4 loop
                    for y in 0 to 4 loop
                        a_s(x, y) <= b_s(x, y) xor (not b_s((x + 1) mod 5, y) and b_s((x + 2) mod 5, y));
                    end loop;
                end loop;
            elsif iota = '1' then
                a_s(0, 0) <= a_s(0, 0) xor RC(to_integer(round));
            end if;

            if theta1 = '1' then
                for x in 0 to 4 loop
                    c_s(x) <= a_s(x, 0) xor a_s(x, 1) xor a_s(x, 2) xor a_s(x, 3) xor a_s(x, 4);
                end loop;
            end if;
            if rhopi = '1' then
                for x in 0 to 4 loop
                    for y in 0 to 4 loop
                        b_s(y, (2 * x + 3 * y) mod 5) <= rotl(a_s(x, y), RHO(x, y));
                    end loop;
                end loop;
            end if;
        end if;
    end process;

    -- Input block, filled until 17 lanes
    fill: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' or blk_clear = '1' then
                fill_s <= (others => '0');
            elsif in_valid = '1' and fill_s /= 17 then
                blk_s(to_integer(fill_s)) <= in_data;
                fill_s <= fill_s + 1;
            end if;
        end if;
    end process;

    blk_full <= '1' when fill_s = 17 else '0';
    out_data <= a_s(to_integer(out_sel), 0);

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- SHA3 sequencer: absorb nb blocks, 24 rounds of one cycle each
entity sha3_control is
    port (
        clk       : in  std_logic;
        rst       : in  std_logic;
        start     : in  std_logic;
        nb        : in  std_logic_vector(31 downto 0);
        out_busy  : in  std_logic;
        blk_full  : in  std_logic;

        idle      : out std_logic;
        accept    : out std_logic;
        load      : out std_logic;
        result    : out std_logic;

        clear     : out std_logic;
        blk_clear : out std_logic;
        absorb    : out std_logic;
        rnd       : out std_logic;
        round     : out unsigned(4 downto 0)
    );
end entity;

architecture rtl of sha3_control is
    type fsm_t is (S_IDLE, S_LOAD, S_ROUND, S_FINISH);
    signal state_s  : fsm_t;
    signal round_s  : unsigned(4 downto 0);
    signal blocks_s : unsigned(31 downto 0);
    signal nb_s     : unsigned(31 downto 0);
    signal accept_s : std_logic;
begin

    accept_s <= '1' when state_s = S_IDLE and start = '1' and out_busy = '0' else '0';

    fsm: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                state_s <= S_IDLE;
            else
                case state_s is
                    when S_IDLE =>
                        if accept_s = '1' then
                            nb_s     <= unsigned(nb);
                            blocks_s <= (others => '0');
                            state_s  <= S_LOAD;
                        end if;
                    when S_LOAD =>
                        round_s <= (others => '0');
                        if blk_full = '1' then
                            state_s <= S_ROUND;
                        end if;
                    when S_ROUND =>
                        round_s <= round_s + 1;
                        if round_s = 23 then
                            blocks_s <= blocks_s + 1;
                            state_s  <= S_FINISH;
                        end if;
                    when S_FINISH =>
                        if blocks_s = nb_s then
                            state_s <= S_IDLE;
                        else
                            state_s <= S_LOAD;
                        end if;
                end case;
            end if;
        end if;
    end process;

    idle      <= '1' when state_s = S_IDLE else '0';
    accept    <= accept_s;
    load      <= '1' when state_s = S_LOAD else '0';
    result    <= '1' when state_s = S_FINISH and blocks_s = nb_s else '0';
    clear     <= accept_s;
    blk_clear <= '1' when accept_s = '1' or (state_s = S_FINISH and blocks_s /= nb_s) else '0';
    absorb    <= '1' when state_s = S_LOAD and blk_full = '1' else '0';
    rnd       <= '1' when state_s = S_ROUND else '0';
    round     <= round_s;

end architecture;

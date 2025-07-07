library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity free_list_fifo is
    generic (
        DEPTH : natural := 64;
        WIDTH : natural := 6  -- broj bitova za indeks
    );
    port (
        clk       : in  std_logic;
        reset     : in  std_logic;
        push      : in  std_logic;
        push_data : in  std_logic_vector(WIDTH-1 downto 0);
        pop       : in  std_logic;
        pop_data  : out std_logic_vector(WIDTH-1 downto 0);
        empty     : out std_logic;
        full      : out std_logic
    );
end free_list_fifo;

architecture Behavioral of free_list_fifo is
    type mem_type is array(0 to DEPTH-1) of std_logic_vector(WIDTH-1 downto 0);
    
    signal mem   : mem_type := (others => (others => '0'));
    signal wr_ptr, rd_ptr : integer range 0 to DEPTH-1 := 0;
    signal count : integer range 0 to DEPTH := 0;
    signal full_s, empty_s : std_logic;
    
begin

    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                wr_ptr <= 0;
                rd_ptr <= 0;
                count  <= 0;
            else
                if (push = '1') and (full_s = '0') then
                    mem(wr_ptr) <= push_data;
                    wr_ptr <= (wr_ptr + 1) mod DEPTH;
                    count <= count + 1;
                end if;
                if (pop = '1') and (empty_s = '0') then
                    rd_ptr <= (rd_ptr + 1) mod DEPTH;
                    count <= count - 1;
                end if;
            end if;
        end if;
    end process;

    pop_data <= mem(rd_ptr);
    full_s   <= '1' when count = DEPTH else '0';
    full     <= full_s;
    empty_s  <= '1' when count = 0 else '0';
    empty    <= empty_s;
    
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity free_list_fifo is
    generic (
        DEPTH       : natural := 64;
        DATA_WIDTH  : natural := 6
    );
    port (
        clk       : in  std_logic;
        reset     : in  std_logic;
        push      : in  std_logic;
        push_data : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        pop       : in  std_logic;
        pop_data  : out std_logic_vector(DATA_WIDTH-1 downto 0);
        empty     : out std_logic;
        full      : out std_logic
    );
end free_list_fifo;

architecture Behavioral of free_list_fifo is
    type mem_type is array(0 to DEPTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Function to initialize memory with sequential values (0,1,2,...)
    function init_mem return mem_type is
        variable temp_mem : mem_type;
    begin
        for i in 0 to DEPTH-1 loop
            temp_mem(i) := std_logic_vector(to_unsigned(i, DATA_WIDTH));
        end loop;
        return temp_mem;
    end function;
    
    signal mem   : mem_type := init_mem;  -- Initialize directly with all physical registers
    signal wr_ptr, rd_ptr : integer range 0 to DEPTH-1 := 0;
    signal count : integer range 0 to DEPTH := DEPTH;  -- Start FULL (all registers available)
    signal full_s, empty_s : std_logic;
    
begin

    process(clk)
    begin
        if rising_edge(clk) then
            if (reset = '0') then
                -- Reset to FULL state (all physical registers free)
                wr_ptr <= 0;
                rd_ptr <= 0;
                count  <= DEPTH;  -- FULL
                -- mem NOT reset - retains initial values
            else
                -- Push operation (return register to free list on commit)
                if (push = '1') and (full_s = '0') then
                    mem(wr_ptr) <= push_data;
                    wr_ptr <= (wr_ptr + 1) mod DEPTH;
                    count <= count + 1;
                end if;
                
                -- Pop operation (take free register for rename)
                if (pop = '1') and (empty_s = '0') then
                    rd_ptr <= (rd_ptr + 1) mod DEPTH;
                    count <= count - 1;
                end if;
            end if;
        end if;
    end process;

    -- Outputs
    pop_data <= mem(rd_ptr);
    full_s   <= '1' when count = DEPTH else '0';
    full     <= full_s;
    empty_s  <= '1' when count = 0 else '0';
    empty    <= empty_s;
    
end Behavioral;
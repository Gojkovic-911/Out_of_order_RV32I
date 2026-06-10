library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity free_list_fifo is
    generic (
        DEPTH       : natural := 64;
        DATA_WIDTH  : natural := 6
    );
    port (
        clk         : in  std_logic;
        reset       : in  std_logic;
        push        : in  std_logic;
        push_data   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        pop         : in  std_logic;
        snapshot_i  : in  std_logic;
        flush_i     : in  std_logic;
        pop_data    : out std_logic_vector(DATA_WIDTH-1 downto 0);
        empty       : out std_logic;
        full        : out std_logic
    );
end free_list_fifo;

architecture Behavioral of free_list_fifo is
    
    type fifo_entry is record
        value : std_logic_vector(DATA_WIDTH-1 downto 0);
        valid : std_logic;
    end record;
    
    type mem_type is array(0 to DEPTH-1) of fifo_entry;
    
    -- Function to initialize memory with sequential values (0,1,2,...)
    function init_mem return mem_type is
        variable temp_mem : mem_type;
    begin
        for i in 1 to DEPTH-1 loop
            temp_mem(i).value := std_logic_vector(to_unsigned(i, DATA_WIDTH));
            temp_mem(i).valid := '1';
        end loop;
        temp_mem(0).value := (others => '0');
        temp_mem(0).valid := '1';
        return temp_mem;
    end function;
    
    signal mem              : mem_type := init_mem;  -- Initialize directly with all physical registers
    signal mem_snap         : mem_type := init_mem;  -- Initialize directly with all physical registers
    signal wr_ptr, rd_ptr   : integer range 0 to DEPTH-1 := 0;
    signal full_s, empty_s  : std_logic;
    signal push_valid       : std_logic;
    
begin
    
    push_valid <= '1' when push_data /= std_logic_vector(to_unsigned(0, DATA_WIDTH)) else '0';
    
    process(clk)
    begin
        if rising_edge(clk) then
            if (reset = '0') then
                -- Reset to FULL state (all but 0 physical registers free)
                wr_ptr <= 1;
                rd_ptr <= 1;
                -- mem <= ( others => ( valid => '0', value => (others => '0')));
            else
                -- Push operation (return register to free list on commit)
                if push = '1' and full_s = '0' and push_valid = '1' and mem(wr_ptr).valid = '0' then
                    mem(wr_ptr).value <= push_data;
                    mem(wr_ptr).valid <= '1';
                    
                    if(wr_ptr = DEPTH-1) then
                        wr_ptr  <= 1;
                    else
                        wr_ptr <= wr_ptr + 1;
                    end if;    
                end if;
                
                -- Pop operation (take free register for rename)
                if pop = '1' and empty_s = '0' and mem(rd_ptr).valid = '1' then
                    mem(rd_ptr).valid <= '0';
                    rd_ptr <= (rd_ptr + 1) mod DEPTH;
                    
                    if(rd_ptr = DEPTH-1) then
                        rd_ptr  <= 1;
                    else
                        rd_ptr <= rd_ptr + 1;
                    end if;    
                end if;
                
                if snapshot_i = '1' then
                    mem_snap <= mem;
                end if;
                
                if flush_i = '1' then
                    mem <= mem_snap;
                end if;
            end if;
        end if;
    end process;
    
    -- Outputs
    -- Synch read ?
    pop_data <= mem(rd_ptr).value;
    full_s   <= '1' when rd_ptr = wr_ptr and mem(wr_ptr).valid = '1' else '0';
    full     <= full_s;
    empty_s  <= '1' when rd_ptr = wr_ptr and mem(wr_ptr).valid = '0' else '0';
    empty    <= empty_s;
    
end Behavioral;
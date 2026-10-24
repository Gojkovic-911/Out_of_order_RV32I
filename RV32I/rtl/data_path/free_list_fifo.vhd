library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Free physical registers
-- Rename pops at rd_ptr, commit pushes the previous mapping at wr_ptr.
-- rd_ptr_commit follows rd_ptr at commit: rename and commit go in the same
-- (program) order, so on a misprediction moving rd_ptr back to rd_ptr_commit
-- returns every register taken by the flushed instructions.
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
        commit_pop  : in  std_logic;    -- committed instruction had popped a register
        flush_i     : in  std_logic;
        pop_data    : out std_logic_vector(DATA_WIDTH-1 downto 0);
        empty       : out std_logic;
        full        : out std_logic
    );
end free_list_fifo;

architecture Behavioral of free_list_fifo is

    -- Physical register 0 is never free
    constant SLOTS : natural := DEPTH - 1;

    type mem_type is array(0 to SLOTS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);

    -- All physical registers but 0 are free after reset
    function init_mem return mem_type is
        variable temp_mem : mem_type;
    begin
        for i in 0 to SLOTS-1 loop
            temp_mem(i) := std_logic_vector(to_unsigned(i + 1, DATA_WIDTH));
        end loop;
        return temp_mem;
    end function;

    function next_ptr(p : integer) return integer is
    begin
        return (p + 1) mod SLOTS;
    end function;

    signal mem                  : mem_type := init_mem;
    signal wr_ptr               : integer range 0 to SLOTS-1 := 0;
    signal rd_ptr               : integer range 0 to SLOTS-1 := 0;
    signal rd_ptr_commit        : integer range 0 to SLOTS-1 := 0;
    signal count                : integer range 0 to SLOTS := SLOTS;   -- free for rename
    signal count_commit         : integer range 0 to SLOTS := SLOTS;   -- free at commit
    signal push_valid           : std_logic;

begin

    push_valid <= '1' when push_data /= std_logic_vector(to_unsigned(0, DATA_WIDTH)) else '0';

    process(clk)
        variable push_v         : boolean;
        variable count_v        : integer range 0 to SLOTS;
        variable count_commit_v : integer range 0 to SLOTS;
        variable rd_ptr_v       : integer range 0 to SLOTS-1;
        variable rd_ptr_commit_v: integer range 0 to SLOTS-1;
    begin
        if rising_edge(clk) then
            if (reset = '0') then
                mem             <= init_mem;
                wr_ptr          <= 0;
                rd_ptr          <= 0;
                rd_ptr_commit   <= 0;
                count           <= SLOTS;
                count_commit    <= SLOTS;
            else
                count_v         := count;
                count_commit_v  := count_commit;
                rd_ptr_v        := rd_ptr;
                rd_ptr_commit_v := rd_ptr_commit;

                -- Push operation (return register to free list on commit)
                push_v := push = '1' and push_valid = '1' and count_commit < SLOTS;
                if push_v then
                    mem(wr_ptr)     <= push_data;
                    wr_ptr          <= next_ptr(wr_ptr);
                    count_v         := count_v + 1;
                    count_commit_v  := count_commit_v + 1;
                end if;

                -- Pop operation (take free register for rename)
                if pop = '1' and count > 0 then
                    rd_ptr_v        := next_ptr(rd_ptr);
                    count_v         := count_v - 1;
                end if;

                -- Commit of an instruction that took a register
                if commit_pop = '1' then
                    rd_ptr_commit_v := next_ptr(rd_ptr_commit);
                    count_commit_v  := count_commit_v - 1;
                end if;

                -- Missprediction: give back the registers of all flushed instructions
                if flush_i = '1' then
                    rd_ptr_v        := rd_ptr_commit_v;
                    count_v         := count_commit_v;
                end if;

                rd_ptr          <= rd_ptr_v;
                rd_ptr_commit   <= rd_ptr_commit_v;
                count           <= count_v;
                count_commit    <= count_commit_v;
            end if;
        end if;
    end process;

    -- Outputs
    pop_data <= mem(rd_ptr);
    empty    <= '1' when count = 0 else '0';
    full     <= '1' when count = SLOTS else '0';

end Behavioral;

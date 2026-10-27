library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Branch decision on IS/EX operands
entity branch_unit is
    generic (
        DATA_WIDTH      : natural := 32;
        ROB_ADDR_BITS   : natural := 6
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;
        flush_i         : in  std_logic;

        valid_i         : in  std_logic;
        rs1_data_i      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rs2_data_i      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        subtype_i       : in  std_logic_vector(4 downto 0);
        rob_idx_i       : in  std_logic_vector(ROB_ADDR_BITS-1 downto 0);

        -- Branch outcome to ROB
        taken_valid_o   : out std_logic;
        taken_o         : out std_logic;
        taken_idx_o     : out std_logic_vector(ROB_ADDR_BITS-1 downto 0)
    );
end entity;

architecture Behavioral of branch_unit is
    signal taken_s          : std_logic;
    signal taken_valid_r_s  : std_logic;
begin

    u_bdu: entity work.branch_decision_unit
        generic map (
            DATA_WIDTH => DATA_WIDTH
        )
        port map (
            branch_condition_o => taken_s,
            a_i                => rs1_data_i,
            b_i                => rs2_data_i,
            funct3_i           => subtype_i
        );

    -- Outcome register, dropped on flush
    outcome_reg: process(clk)
    begin
        if rising_edge(clk) then
            if reset = '0' then
                taken_valid_r_s <= '0';
                taken_o         <= '0';
                taken_idx_o     <= (others => '0');
            else
                taken_valid_r_s <= valid_i and not flush_i;
                taken_o         <= taken_s;
                taken_idx_o     <= rob_idx_i;
            end if;
        end if;
    end process;

    taken_valid_o <= taken_valid_r_s and not flush_i;

end Behavioral;

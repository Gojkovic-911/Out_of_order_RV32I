library ieee;
use ieee.std_logic_1164.all;

-- CPU platform, board clock and reset button
entity platform_tb is
   generic (BOARD_CLK_MHZ : positive := 125);
end entity;

architecture Behavioral of platform_tb is
   constant HALF_PERIOD : time := (500.0 / real(BOARD_CLK_MHZ)) * 1 ns;
   signal clk  : std_logic := '0';
   signal rst  : std_logic := '1';
   signal gpio : std_logic_vector(3 downto 0);
begin

   dut : entity work.design_1_wrapper
      port map (
         clk  => clk,
         rst  => rst,
         gpio => gpio);

   clk <= not clk after HALF_PERIOD;
   rst <= '0' after 1 us;

end Behavioral;

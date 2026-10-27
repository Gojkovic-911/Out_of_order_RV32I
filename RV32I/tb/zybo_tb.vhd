library ieee;
use ieee.std_logic_1164.all;

-- Zybo block design, board clock and BTN0
entity zybo_tb is
end entity;

architecture Behavioral of zybo_tb is
   signal clk  : std_logic := '0';
   signal rst  : std_logic := '1';
   signal gpio : std_logic_vector(3 downto 0);
begin

   dut : entity work.design_1_wrapper
      port map (
         clk  => clk,
         rst  => rst,
         gpio => gpio);

   clk <= not clk after 4 ns;
   rst <= '0' after 1 us;

end Behavioral;

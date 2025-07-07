LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
use ieee.math_real.all;
use work.alu_ops_pkg.all;

entity branch_decision_unit is
    generic(
        DATA_WIDTH : NATURAL := 32);
    Port (  branch_condition_o  : out std_logic;
            a_i                 : in STD_LOGIC_VECTOR(DATA_WIDTH-1 DOWNTO 0); -- first operand
            b_i                 : in STD_LOGIC_VECTOR(DATA_WIDTH-1 DOWNTO 0); -- second operand
            funct3_i            : in std_logic_vector(2 downto 0)
            );
end branch_decision_unit;

architecture Behavioral of branch_decision_unit is


begin
    process (funct3_i, a_i, b_i)
    begin
    case funct3_i is
        when "000"  =>  -- BEQ | "001"
            if(a_i = b_i) then
                branch_condition_o <= '1';
            else
                branch_condition_o <= '0';
            end if;
            
        when "001" =>   -- BNE
            if(a_i = b_i) then
                branch_condition_o <= '0';
            else
                branch_condition_o <= '1';
            end if;
                
        when "100" =>  -- BLT
            if(signed(a_i) < signed(b_i)) then
                branch_condition_o <= '1';
            else
                branch_condition_o <= '0';
            end if;
            
        when "101" =>  -- BGE
            if(signed(a_i) < signed(b_i)) then
                branch_condition_o <= '0';
            else
                branch_condition_o <= '1';
            end if;
            
        when "110" =>  -- BLTU
            if (unsigned(a_i) < unsigned(b_i)) then
                branch_condition_o <= '1';
            else
                branch_condition_o <= '0';
            end if;  
            
        when "111" =>  -- BGEU
            if (unsigned(a_i) < unsigned(b_i)) then
                branch_condition_o <= '0';
            else
                branch_condition_o <= '1';
            end if;  
        when others =>  
            branch_condition_o <= '0';
        end case;
    end process;
end Behavioral;

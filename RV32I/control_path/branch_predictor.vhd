library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.instr_types_pkg.all;

entity branch_predictor is
    generic (
        DATA_WIDTH     : natural := 32
    );
    Port ( 
        fetch_intr_i    : in std_logic_vector(DATA_WIDTH-1 downto 0);
        is_jump_o       : out std_logic;
        is_spec_o       : out std_logic
    );
end branch_predictor;

architecture Behavioral of branch_predictor is

    signal opcode  : std_logic_vector(6 downto 0);
begin

    opcode      <= fetch_intr_i(6 downto 0);

    process(opcode) is
    begin
        is_jump_o    <= '0';
        is_spec_o    <= '0';
        if opcode = OPCODE_BRANCH then
            -- BRANCH
            is_jump_o       <= '1';
            is_spec_o       <= '1';
        
        elsif opcode = OPCODE_JAL then
            -- JAL
            is_jump_o       <= '1';

        elsif opcode = OPCODE_JALR then
            -- JALR
            is_spec_o       <= '1';
            
        end if;
    end process;
end Behavioral;

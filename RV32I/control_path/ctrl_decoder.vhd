library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ctrl_decoder is
    port (
        --************ Opcode polje instrukcije************
        opcode_i            : in  std_logic_vector (6 downto 0);
        --************ Kontrolni signali*******************
        branch_o            : out std_logic;
        mem_to_reg_o        : out std_logic;
        alu_src_o           : out std_logic;
        rd_we_o             : out std_logic;
        load_instr_o        : out std_logic;
        store_instr_o       : out std_logic;
        alu_2bit_op_o       : out std_logic_vector(1 downto 0);
        rd_data_source_o    : out std_logic_vector(1 downto 0)
        
        );
end entity;

architecture behavioral of ctrl_decoder is
begin

   contol_dec : process(opcode_i)is
   begin
      --***Podrazumevane vrednost***
      branch_o       <= '0';
      mem_to_reg_o   <= '0';
      alu_src_o      <= '0';
      rd_we_o        <= '0';
      load_instr_o   <= '0'; 
      store_instr_o  <= '0';      
      alu_2bit_op_o  <= "00";
      rd_data_source_o <= "00";
      --****************************      
      case opcode_i(6 downto 2) is
         when "00000" =>                -- LOAD, 5v ~ funct3
            alu_2bit_op_o    <= "00";
            mem_to_reg_o     <= '1';
            alu_src_o        <= '1';
            rd_we_o          <= '1';
            load_instr_o     <= '1';
         when "01000" =>                -- STORE, 3v ~ funct3
            alu_2bit_op_o    <= "00";
            alu_src_o        <= '1';
            store_instr_o    <= '1';
         when "01100" =>                -- R type, 
            alu_2bit_op_o    <= "01";
            rd_we_o          <= '1';
         when "00100" =>                -- I type
            alu_2bit_op_o    <= "10";
            alu_src_o        <= '1';
            rd_we_o          <= '1';
         when "11000" =>                -- B type
            alu_2bit_op_o    <= "00";
            branch_o         <= '1';
         when "11011" =>                -- J type  JAL
            rd_data_source_o <= "01";
            rd_we_o          <= '1';
         when "11001" =>                -- J type  JALR
            alu_2bit_op_o    <= "00";
            rd_data_source_o <= "01";
            rd_we_o          <= '1';
         when "01101" =>                -- U type LUI
            rd_data_source_o <= "10";
            alu_src_o        <= '1';
            rd_we_o          <= '1';
         when "00101" =>                -- U type AUIPC
            rd_data_source_o <= "11";
            rd_we_o          <= '1';
         when others =>
      end case;
   end process;

end architecture;

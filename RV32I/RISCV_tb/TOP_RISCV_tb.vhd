library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;
use work.txt_util.all;


entity TOP_RISCV_tb is
generic (
        Vivado_version    :std_logic  := '0'
    );
-- port ();
end entity;

architecture Behavioral of TOP_RISCV_tb is
   -- Operand za pristup asemblerskom kodu programa
   file RISCV_instructions             : text open read_mode is "../../../../../RISCV_tb/assembly_code.txt";
   -- Globalni signali
   signal clk                          : std_logic := '0';
   signal reset                        : std_logic;
   -- Signali memorije za instrukcije
   signal ena_instr_s, enb_instr_s     : std_logic;
   signal wea_instr_s, web_instr_s     : std_logic_vector(3 downto 0);
   signal addra_instr_s, addrb_instr_s : std_logic_vector(9 downto 0);
   signal dina_instr_s, dinb_instr_s   : std_logic_vector(31 downto 0);
   signal douta_instr_s, doutb_instr_s : std_logic_vector(31 downto 0);
   signal addrb_instr_32_s             : std_logic_vector(31 downto 0);
   -- Signali memorije za podatke
   signal ena_data_s, enb_data_s       : std_logic;
   signal wea_data_s, web_data_s       : std_logic_vector(3 downto 0);
   signal addra_data_s, addrb_data_s   : std_logic_vector(9 downto 0);
   signal dina_data_s, dinb_data_s     : std_logic_vector(31 downto 0);
   signal douta_data_s, doutb_data_s   : std_logic_vector(31 downto 0);
   signal addra_data_32_s              : std_logic_vector(31 downto 0);

begin

   -- Memorija za instrukcije
   -- Pristup A : Koristi se za inicijalizaciju memorije za instrukcije
   -- Pristup B : Koristi se za citanje instrukcija od strane procesora 
   -- Konstante:
   ena_instr_s   <= '1';
   enb_instr_s   <= '1';
   addrb_instr_s <= addrb_instr_32_s(9 downto 0);
   web_instr_s   <= (others => '0');
   dinb_instr_s  <= (others => '0');
   -- Instanca:
   instruction_mem : entity work.BRAM(behavioral)
      generic map(WADDR => 10)
      port map (clk      => clk,
                -- pristup A
                en_a_i   => ena_instr_s,
                we_a_i   => wea_instr_s,
                addr_a_i => addra_instr_s,
                data_a_i => dina_instr_s,
                data_a_o => douta_instr_s,
                -- pristup B
                en_b_i   => enb_instr_s,
                we_b_i   => web_instr_s,
                addr_b_i => addrb_instr_s,
                data_b_i => dinb_instr_s,
                data_b_o => doutb_instr_s);


   -- Memorija za podatke
   -- Pristup A : Koristi procesor kako bi upisivao i citao podatke
   -- Pristup B : Ne koristi se
   -- Konstante:
   addra_data_s <= addra_data_32_s(9 downto 0);
   addrb_data_s <= (others => '0');
   dinb_data_s  <= (others => '0');
   ena_data_s   <= '1';
   enb_data_s   <= '1';
   -- Instanca:
   data_mem : entity work.BRAM(behavioral)
      generic map(WADDR => 10)
      port map (clk      => clk,
                -- pristup A
                en_a_i   => ena_data_s,
                we_a_i   => wea_data_s,
                addr_a_i => addra_data_s,
                data_a_i => dina_data_s,
                data_a_o => douta_data_s,
                -- pristup B
                en_b_i   => enb_data_s,
                we_b_i   => web_data_s,
                addr_b_i => addrb_data_s,
                data_b_i => dinb_data_s,
                data_b_o => doutb_data_s);


   -- Top Modul - RISCV procesor jezgro
   TOP_RISCV_1 : entity work.TOP_RISCV
      port map (
         clk   => clk,
         reset => reset,

         instr_mem_data_i   => doutb_instr_s,
         instr_mem_addr_o   => addrb_instr_32_s,

         data_mem_be_o      => wea_data_s,
         data_mem_addr_o    => addra_data_32_s,
         data_mem_rdata_i   => douta_data_s,
         data_mem_wdata_o   => dina_data_s);

   -- Inicijalizacija memorije za instrukcije
   -- Program koji ce procesor izvrsavati se ucitava u memoriju
   read_file_proc : process
      variable row : line;
      variable i   : integer := 0;
   begin
      reset       <= '0';
      wea_instr_s <= (others => '1');
      wait until rising_edge(clk);
      while (not endfile(RISCV_instructions))loop
         readline(RISCV_instructions, row);
         if (row'length > 0) then
            addra_instr_s <= std_logic_vector(to_unsigned(i, 10));
            dina_instr_s  <= to_std_logic_vector(string(row));
         end if;
         i             := i + 4;
         wait until rising_edge(clk);
      end loop;
      wea_instr_s <= (others => '0');
      reset       <= '1' after 20 ns;
      wait;
   end process;

   -- klok signal generator
   clk_proc : process
   begin
      clk <= '1', '0' after 100 ns;
      wait for 200 ns;
   end process;
   
    dump_proc: process
        file dump_file : text open write_mode is "register_dump.txt";
        variable line_out : line;
        variable timestamp : time;
    begin
        -- Sačekaj kraj simulacije (kada sve instrukcije završe)
        wait for 25 us;
        
        timestamp := now;
        
        -- Otvori fajl za pisanje
        file_open(dump_file, "register_dump.txt", write_mode);
        
        -- Header
        write(line_out, string'("============================================="));
        writeline(dump_file, line_out);
        write(line_out, string'("REGISTER DUMP AT TIME: "));
        write(line_out, timestamp);
        writeline(dump_file, line_out);
        write(line_out, string'("============================================="));
        writeline(dump_file, line_out);
        writeline(dump_file, line_out);
        
        -- ==================================================
        -- 1. COMMIT RENAME TABLE (mapiranje arhitektonskih -> fizičkih registara)
        -- ==================================================
        write(line_out, string'("=== COMMIT RENAME TABLE ==="));
        writeline(dump_file, line_out);
        write(line_out, string'("ARCH REG -> PHYS REG"));
        writeline(dump_file, line_out);
        write(line_out, string'("------------------------"));
        writeline(dump_file, line_out);
        
        for i in 0 to 31 loop
            write(line_out, string'("x"));
            write(line_out, i);
            write(line_out, string'("     -> reg_"));
            write(line_out, to_integer(unsigned(
                TOP_RISCV_1.data_path_1.u_renaming_module.commit_rename_table_s(i)
            )));
            writeline(dump_file, line_out);
        end loop;
        
        writeline(dump_file, line_out);
        writeline(dump_file, line_out);
        
        -- ==================================================
        -- 2. PHYSICAL REGISTERS (stvarne vrijednosti podataka)
        -- ==================================================
        write(line_out, string'("=== PHYSICAL REGISTERS ==="));
        writeline(dump_file, line_out);
        write(line_out, string'("PHYS REG -> VALUE (hex)"));
        writeline(dump_file, line_out);
        write(line_out, string'("------------------------"));
        writeline(dump_file, line_out);
        
        for i in 0 to 63 loop
            write(line_out, string'("reg_"));
            write(line_out, i);
            write(line_out, string'("  -> 0x"));
            write(line_out, to_hex_string(
                TOP_RISCV_1.data_path_1.phys_regs_s(i)
            ));
            writeline(dump_file, line_out);
        end loop;
        
        writeline(dump_file, line_out);
        writeline(dump_file, line_out);
        
        -- ==================================================
        -- 3. SAMO REGISTRI KOJI SU KORIŠTENI (NON-ZERO)
        -- ==================================================
        write(line_out, string'("=== NON-ZERO PHYSICAL REGISTERS ONLY ==="));
        writeline(dump_file, line_out);
        write(line_out, string'("PHYS REG -> VALUE (hex)"));
        writeline(dump_file, line_out);
        write(line_out, string'("------------------------"));
        writeline(dump_file, line_out);
        
        for i in 0 to 63 loop
            if unsigned(TOP_RISCV_1.data_path_1.phys_regs_s(i)) /= 0 then
                write(line_out, string'("reg_"));
                write(line_out, i);
                write(line_out, string'("  -> 0x"));
                write(line_out, to_hex_string(
                    TOP_RISCV_1.data_path_1.phys_regs_s(i)
                ));
                writeline(dump_file, line_out);
            end if;
        end loop;
        
        writeline(dump_file, line_out);
        write(line_out, string'("============================================="));
        writeline(dump_file, line_out);
        write(line_out, string'("END OF DUMP"));
        writeline(dump_file, line_out);
        write(line_out, string'("============================================="));
        writeline(dump_file, line_out);
        
        -- Zatvori fajl
        file_close(dump_file);
        
        -- Report
        report "Register dump written to register_dump.txt at time " & time'image(timestamp);
        
        wait; -- završi proces
    end process dump_proc;


end architecture;

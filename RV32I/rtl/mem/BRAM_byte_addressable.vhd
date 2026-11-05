library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity BRAM is
   generic
      (
         -- Byte address bits, 2^ADDR_BITS bytes
         ADDR_BITS  : natural  := 10;
         -- Read latency in cycles: 1, 2 with the BRAM output register, more adds fabric registers
         RD_LATENCY : positive := 1;
         -- Port A: MIN..MAX random wait cycles on top of RD_LATENCY (0..0: fixed latency)
         MIN_WAIT   : natural  := 0;
         MAX_WAIT   : natural  := 0;
         SEED       : natural  := 16#ACE1#;
         -- Initial contents from file
         INIT_FILE  : string   := "";
         -- Simulation: reload INIT_FILE on the first clock (restart, no new elaboration)
         SIM_LOAD   : boolean  := false
         );
   port
      (
         clk      : in  std_logic;
         resetn_i : in  std_logic;
         -- Port A: request valid/ready, response pulse rsp with data_a_o
         req_a_i  : in  std_logic;
         ready_a_o : out std_logic;
         rsp_a_o  : out std_logic;
         en_b_i   : in  std_logic;
         data_a_i : in  std_logic_vector(31 downto 0);
         data_b_i : in  std_logic_vector(31 downto 0);
         addr_a_i : in  std_logic_vector(ADDR_BITS - 1 downto 0);
         addr_b_i : in  std_logic_vector(ADDR_BITS - 1 downto 0);
         we_a_i   : in  std_logic_vector(3 downto 0);
         we_b_i   : in  std_logic_vector(3 downto 0);
         data_a_o : out std_logic_vector(31 downto 0);
         data_b_o : out std_logic_vector(31 downto 0)
         );

end BRAM;

architecture behavioral of BRAM is
   -- Word organized, byte enables
   constant WORDS : natural := 2**(ADDR_BITS - 2);
   type ram_type is array(0 to WORDS - 1) of std_logic_vector(31 downto 0);

   impure function read_init_file(name : string) return ram_type is
      file     f    : text open read_mode is name;
      variable row  : line;
      variable word : bit_vector(31 downto 0);
      variable ram  : ram_type := (others => (others => '0'));
      variable a    : natural  := 0;
   begin
      while (not endfile(f)) and (a < WORDS) loop
         readline(f, row);
         if row'length > 0 then
            read(row, word);
            ram(a) := to_stdlogicvector(word);
            a := a + 1;
         end if;
      end loop;
      return ram;
   end function;

   impure function init_ram return ram_type is
   begin
      if INIT_FILE = "" then
         return (others => (others => '0'));
      end if;
      return read_init_file(INIT_FILE);
   end function;

   -- True dual port, byte writes, read first (Vivado UG901 template)
   shared variable ram_v : ram_type := init_ram;
   signal ram_a_s  : std_logic_vector(31 downto 0);
   signal ram_b_s  : std_logic_vector(31 downto 0);
   signal en_a_s   : std_logic;

begin

   port_a: process(clk)
      variable a_idx : natural range 0 to WORDS - 1;
      -- pragma translate_off
      variable loaded : boolean := false;
      -- pragma translate_on
   begin
      if(rising_edge(clk)) then
         -- pragma translate_off
         if SIM_LOAD and not loaded then
            loaded := true;
            ram_v  := init_ram;
         end if;
         -- pragma translate_on
         a_idx := to_integer(unsigned(addr_a_i(ADDR_BITS - 1 downto 2)));
         if(en_a_s = '1') then
            ram_a_s <= ram_v(a_idx);
            for i in 0 to 3 loop
               if(we_a_i(i) = '1') then
                  ram_v(a_idx)(8*i + 7 downto 8*i) := data_a_i(8*i + 7 downto 8*i);
               end if;
            end loop;
         end if;
      end if;
   end process;

   port_b: process(clk)
      variable b_idx : natural range 0 to WORDS - 1;
   begin
      if(rising_edge(clk)) then
         b_idx := to_integer(unsigned(addr_b_i(ADDR_BITS - 1 downto 2)));
         if(en_b_i = '1') then
            ram_b_s <= ram_v(b_idx);
            for i in 0 to 3 loop
               if(we_b_i(i) = '1') then
                  ram_v(b_idx)(8*i + 7 downto 8*i) := data_b_i(8*i + 7 downto 8*i);
               end if;
            end loop;
         end if;
      end if;
   end process;

   -- Fixed latency: accept every cycle, rsp after RD_LATENCY
   fixed: if MIN_WAIT = 0 and MAX_WAIT = 0 generate
      signal pipe_s : std_logic_vector(RD_LATENCY-1 downto 0) := (others => '0');
   begin
      en_a_s    <= req_a_i;
      ready_a_o <= '1';
      rsp_a_o   <= pipe_s(RD_LATENCY-1);
      process(clk)
      begin
         if rising_edge(clk) then
            if resetn_i = '0' then
               pipe_s <= (others => '0');
            elsif RD_LATENCY = 1 then
               pipe_s(0) <= req_a_i;
            else
               pipe_s <= pipe_s(RD_LATENCY-2 downto 0) & req_a_i;
            end if;
         end if;
      end process;
   end generate;

   -- Variable latency: one request at a time, rsp after RD_LATENCY + LFSR wait
   variable_wait: if MIN_WAIT /= 0 or MAX_WAIT /= 0 generate
      signal busy_s : std_logic := '0';
      signal rsp_s  : std_logic := '0';
      signal cnt_s  : natural range 0 to RD_LATENCY + MAX_WAIT + MIN_WAIT;
   begin
      en_a_s    <= req_a_i and not busy_s;
      ready_a_o <= not busy_s;
      rsp_a_o   <= rsp_s;
      process(clk)
         variable lfsr : unsigned(15 downto 0) := to_unsigned(SEED, 16);
         variable w    : natural;
      begin
         if rising_edge(clk) then
            rsp_s <= '0';
            if resetn_i = '0' then
               busy_s <= '0';
            elsif busy_s = '0' then
               if req_a_i = '1' then
                  lfsr := lfsr(14 downto 0) & (lfsr(15) xor lfsr(13) xor lfsr(12) xor lfsr(10));
                  w := MIN_WAIT;
                  if MAX_WAIT > MIN_WAIT then
                     w := MIN_WAIT + to_integer(lfsr(7 downto 0)) mod (MAX_WAIT - MIN_WAIT + 1);
                  end if;
                  w := w + RD_LATENCY - 1;
                  busy_s <= '1';
                  if w = 0 then
                     rsp_s <= '1';
                  else
                     cnt_s <= w;
                  end if;
               end if;
            elsif rsp_s = '1' then
               busy_s <= '0';
            else
               cnt_s <= cnt_s - 1;
               if cnt_s = 1 then
                  rsp_s <= '1';
               end if;
            end if;
         end if;
      end process;
   end generate;

   -- Optional output register (BRAM DOA_REG/DOB_REG)
   out_direct: if RD_LATENCY = 1 generate
      data_a_o <= ram_a_s;
      data_b_o <= ram_b_s;
   end generate;

   out_reg: if RD_LATENCY >= 2 generate
      type pipe_t is array (2 to RD_LATENCY) of std_logic_vector(31 downto 0);
      signal pipe_a_s, pipe_b_s : pipe_t;
   begin
      process(clk)
      begin
         if rising_edge(clk) then
            pipe_a_s(2) <= ram_a_s;
            pipe_b_s(2) <= ram_b_s;
            for k in 3 to RD_LATENCY loop
               pipe_a_s(k) <= pipe_a_s(k - 1);
               pipe_b_s(k) <= pipe_b_s(k - 1);
            end loop;
         end if;
      end process;
      data_a_o <= pipe_a_s(RD_LATENCY);
      data_b_o <= pipe_b_s(RD_LATENCY);
   end generate;

end behavioral;

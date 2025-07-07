LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
use ieee.math_real.all;
use work.alu_ops_pkg.all;

ENTITY ALU IS
    GENERIC(
        WIDTH : NATURAL := 32);
    PORT(
        a_i    : in STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0); --prvi operand
        b_i    : in STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0); --drugi operand
        op_i   : in STD_LOGIC_VECTOR(4 DOWNTO 0); --port za izbor operacije
        res_o  : out STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0); --rezultat
        zero_o : out STD_LOGIC; -- signal da je rezultat nula
        of_o   : out STD_LOGIC); -- signal da je doslo do prekoracenja opsega
END ALU;

ARCHITECTURE behavioral OF ALU IS

    constant  l2WIDTH : natural := integer(ceil(log2(real(WIDTH))));
    signal    add_res, sub_res, or_res, and_res, xor_res, res_s:  STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0);
    signal    sll_res, srl_res, sra_res: STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0);
    signal    slt_res, sltu_res: STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0);
    signal    a_slt_s, b_slt_s:  STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0);
    signal    slt_par : std_logic_vector(1 downto 0);
   
begin

    -- addition
    add_res <= std_logic_vector(unsigned(a_i) + unsigned(b_i));
    -- subtraction
    sub_res <= std_logic_vector(unsigned(a_i) - unsigned(b_i));
    -- and gate
    and_res <= a_i and b_i;
    -- or gate
    or_res  <= a_i or b_i;  
    -- xor gate
    xor_res <= a_i xor b_i;  
    
    -- SLL 
    process(a_i, b_i)
        variable temp: std_logic_vector(31 downto 0);
    begin
        temp := a_i;    -- input value
        
        -- Barrel shifter
        if b_i(0) = '1' then temp := temp(30 downto 0) & '0'; end if;
        if b_i(1) = '1' then temp := temp(29 downto 0) & "00"; end if;
        if b_i(2) = '1' then temp := temp(27 downto 0) & "0000"; end if;
        if b_i(3) = '1' then temp := temp(23 downto 0) & X"00"; end if;
        if b_i(4) = '1' then temp := temp(15 downto 0) & X"0000"; end if;
        sll_res <= temp;
    end process;

    -- SRL 
    process(a_i, b_i)
        variable temp: std_logic_vector(31 downto 0);
    begin
        temp := a_i;    -- input value
        
        -- Barrel shifter
        if b_i(0) = '1' then temp := '0' & temp(31 downto 1); end if;
        if b_i(1) = '1' then temp := "00" & temp(31 downto 2); end if;
        if b_i(2) = '1' then temp := "0000" & temp(31 downto 4); end if;
        if b_i(3) = '1' then temp := X"00" & temp(31 downto 8); end if;
        if b_i(4) = '1' then temp := X"0000" & temp(31 downto 16); end if;
        srl_res <= temp;
    end process;
   
    -- SRA 
    process(a_i, b_i)
        variable temp: std_logic_vector(31 downto 0);
        variable sign_bit: std_logic;
    begin
        temp     := a_i;      -- input value
        sign_bit := a_i(31);  -- MSB sign
        
        -- Artithmetic Barrel shifter
        if b_i(0) = '1' then temp := sign_bit & temp(31 downto 1); end if;
        if b_i(1) = '1' then temp := (1 to 2 => sign_bit) & temp(31 downto 2); end if;
        if b_i(2) = '1' then temp := (1 to 4 => sign_bit) & temp(31 downto 4); end if;
        if b_i(3) = '1' then temp := (1 to 8 => sign_bit) & temp(31 downto 8); end if;
        if b_i(4) = '1' then temp := (1 to 16 => sign_bit) & temp(31 downto 16); end if;
        sra_res <= temp;
    end process;

    -- SLT
    process(a_i, b_i) is
    begin
        if (signed(a_i) < signed(b_i)) then
            slt_res <= std_logic_vector(to_unsigned(1, WIDTH));
        else
            slt_res <= std_logic_vector(to_unsigned(0, WIDTH));
        end if;
    end process;
   
    -- SLTU
    process (a_i, b_i) is
    begin
        -- default value
        sltu_res <= std_logic_vector(to_unsigned(0, WIDTH));
       
        -- comparison
        if(unsigned(a_i) < unsigned(b_i)) then
            sltu_res <= std_logic_vector(to_unsigned(1, WIDTH));
        else
            sltu_res  <= std_logic_vector(to_unsigned(0, WIDTH));
        end if;
    end process;
   
    -- SELECT RESULT
    res_o <= res_s;
    with op_i select
        res_s <= and_res  when and_op, --and
                 or_res   when or_op, --or
                 add_res  when add_op, --add
                 sub_res  when sub_op, --sub
                 slt_res  when slt_op,
                 sltu_res when sltu_op,
                 sll_res  when sll_op,
                 srl_res  when srl_op,
                 sra_res  when sra_op,
                 xor_res  when xor_op,
                 (others => '1') when others; 
    
    
    -- Postavlja zero_o na 1 ukoliko je rezultat operacije 0
    zero_o <= '1' when res_s = std_logic_vector(to_unsigned(0,WIDTH)) else
             '0';
    
    -- Prekoracenje se desava kada ulazi imaju isti znak, a izlaz razlicit
    of_o <= '1' when ((op_i="00011" and (a_i(WIDTH-1)=b_i(WIDTH-1)) and ((a_i(WIDTH-1) xor res_s(WIDTH-1))='1')) or (op_i="10011" and (a_i(WIDTH-1)=res_s(WIDTH-1)) and ((a_i(WIDTH-1) xor b_i(WIDTH-1))='1'))) else
           '0';


END behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.alu_ops_pkg.all;

entity LSU is
   generic (DATA_WIDTH : positive := 32;
            MEM_BYTES  : natural  := 4 ); -- log2(DATA_WIDTH);
    Port (      
    
      -- Input Data Memory Interface (read/write)
      data_mem_addr_i   : in  std_logic_vector(DATA_WIDTH-1 downto 0); -- input from ALU, used to calculate byte enable signal
      data_mem_wdata_i  : in  std_logic_vector(DATA_WIDTH-1 downto 0); -- just passed because the memory can take unaligned address
      data_mem_rdata_o  : out std_logic_vector(DATA_WIDTH-1 downto 0); -- potentially extended data handed to the RF
      
      -- Output Data Memory Interface (read/write)
      data_mem_addr_o   : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Address for data access
      data_mem_wdata_o  : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data to be written to memory
      data_mem_rdata_i  : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data read from memory
      data_mem_be_o     : out std_logic_vector(MEM_BYTES-1 downto 0);   -- Byte enable (store strobe)
      
      -- Control signals
      lsu_subtype_i     : in  std_logic_vector(4 downto 0)
      );
end LSU;

architecture Behavioral of LSU is
    
    signal extended_load_data_s : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal data_mem_be_s        : std_logic_vector(MEM_BYTES -1 downto 0);
    signal data_mem_wdata_s     : std_logic_vector(DATA_WIDTH-1 downto 0);
    
begin
    
    data_mem_addr_o  <= data_mem_addr_i and x"FFFFFFFC";
    data_mem_wdata_o <= data_mem_wdata_s;
    data_mem_be_o    <= data_mem_be_s;
    data_mem_rdata_o <= extended_load_data_s;
    
   
   -- Store instructions
   process (lsu_subtype_i, data_mem_addr_i) is
   begin
   
       data_mem_be_s    <= (others => '0'); 
       data_mem_wdata_s <= (others => '0'); 
       
       case lsu_subtype_i is
         when SW => 
            if (data_mem_addr_i(1 downto 0) = "00") then
                data_mem_be_s    <= "1111";
                data_mem_wdata_s <= data_mem_wdata_i; 
            else -- EXCEPTION
               data_mem_be_s    <= (others => '0'); 
               data_mem_wdata_s <= (others => '0'); 
            end if;
            
         when SH =>
            case data_mem_addr_i(1 downto 0) is
            when "00" =>
                data_mem_be_s    <= "0011";
                data_mem_wdata_s <= (31 downto 16 => '0') & data_mem_wdata_i(15 downto 0);
            when "10" =>
                data_mem_be_s    <= "1100";
                data_mem_wdata_s <= data_mem_wdata_i(15 downto 0) & (15 downto 0 => '0');

             when others =>  -- EXCEPTION
               data_mem_be_s    <= (others => '0'); 
               data_mem_wdata_s <= (others => '0'); 
           end case;
            
         when SB =>
            case data_mem_addr_i(1 downto 0) is
            when "00"   => 
                data_mem_be_s    <= "0001";
                data_mem_wdata_s <= (31 downto 8 => '0') & data_mem_wdata_i(7 downto 0);
            when "01"   => 
                data_mem_be_s    <= "0010";
                data_mem_wdata_s <= (31 downto 16 => '0') & data_mem_wdata_i(7 downto 0) & (7 downto 0 => '0');
            when "10"   => 
                data_mem_be_s    <= "0100";
                data_mem_wdata_s <= (31 downto 24 => '0') & data_mem_wdata_i(7 downto 0) & (15 downto 0 => '0');
            when "11"   => 
                data_mem_be_s    <= "1000";
                data_mem_wdata_s <= data_mem_wdata_i(7 downto 0) & (23 downto 0 => '0');
            when others => 
               data_mem_be_s    <= (others => '0'); 
               data_mem_wdata_s <= (others => '0'); 
            end case;
           
         when NONE_S =>
           data_mem_be_s    <= (others => '0');
           data_mem_wdata_s <= (others => '0');
           
         when others =>
           data_mem_be_s    <= (others => '0');
           data_mem_wdata_s <= (others => '0');
           
        end case;
    end process;
               
   -- Load instructions
   process (lsu_subtype_i, data_mem_addr_i, data_mem_rdata_i) is
   begin
   
        extended_load_data_s <= (others => '0'); 
        
        case lsu_subtype_i is
         when LW =>
            if (data_mem_addr_i(1 downto 0) = "00") then
                extended_load_data_s <= data_mem_rdata_i; -- passes the read data
            else -- EXCEPTION
            end if;
            
         when LH =>
            case data_mem_addr_i(1 downto 0) is
             when "00"   => 
                extended_load_data_s <= (31 downto 16 => data_mem_rdata_i(15)) & data_mem_rdata_i(15 downto 0);
             when "10"   => 
                extended_load_data_s <= (31 downto 16 => data_mem_rdata_i(31)) & data_mem_rdata_i(31 downto 16); 
             when others =>  -- EXCEPTION
                extended_load_data_s <= (others => '0');
           end case;   
           
         when LHU =>
            case data_mem_addr_i(1 downto 0) is
             when "00"   => 
                extended_load_data_s <= (31 downto 16 => '0') & data_mem_rdata_i(15 downto 0); 
             when "10"   => 
                extended_load_data_s <= (31 downto 16 => '0') & data_mem_rdata_i(31 downto 16);  
             when others =>  -- EXCEPTION
                extended_load_data_s <= (others => '0'); 
           end case;   
           
         when LB =>
           case data_mem_addr_i(1 downto 0) is
             when "00"   => 
                extended_load_data_s <= (31 downto 8 => data_mem_rdata_i(7))  & data_mem_rdata_i(7 downto 0);  
             when "01"   => 
                extended_load_data_s <= (31 downto 8 => data_mem_rdata_i(15)) & data_mem_rdata_i(15 downto 8);  
             when "10"   => 
                extended_load_data_s <= (31 downto 8 => data_mem_rdata_i(23)) & data_mem_rdata_i(23 downto 16);
             when "11"   => 
                extended_load_data_s <= (31 downto 8 => data_mem_rdata_i(31)) & data_mem_rdata_i(31 downto 24); 
             when others => 
                extended_load_data_s <= (others => '0');
           end case;     
                 
         when LBU =>
           case data_mem_addr_i(1 downto 0) is
             when "00"   => 
                extended_load_data_s <= (31 downto 8 => '0') & data_mem_rdata_i(7 downto 0);  
             when "01"   => 
                extended_load_data_s <= (31 downto 8 => '0') & data_mem_rdata_i(15 downto 8);  
             when "10"   => 
                extended_load_data_s <= (31 downto 8 => '0') & data_mem_rdata_i(23 downto 16);
             when "11"   => 
                extended_load_data_s <= (31 downto 8 => '0') & data_mem_rdata_i(31 downto 24); 
             when others => 
                extended_load_data_s <= (others => '0');
           end case;
           
         when NONE_L =>
            extended_load_data_s <= (others => '0');
            
         when others =>
           extended_load_data_s <= (others => '0');
           
        end case;
    end process;
    
end Behavioral;

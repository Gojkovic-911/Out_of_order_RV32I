library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.alu_ops_pkg.all;
use work.instr_types_pkg.all;

entity control_path is
    generic (
        DATA_WIDTH     : natural := 32
    );
    port (
        clk                 : in  std_logic;
        reset               : in  std_logic;
        
        -- Instruction from data path
        instruction_i           : in  std_logic_vector (31 downto 0);
        
        -- ***************** Control inteface ****************************
        instr_format_o          : out std_logic_vector(2 downto 0); -- Immediate 
        
        fetch_is_jump_o        : out std_logic;
        fetch_is_spec_o        : out std_logic;

        rename_rd_we_o          : out std_logic;    -- Rd used
        rename_rs1_used_o       : out std_logic;    -- rs1 used
        rename_rs2_used_o       : out std_logic;    -- rs2 used
        rename_snapshot_o       : out std_logic;
        
        pc_next_sel_o           : out std_logic;
        
        dispatch_instr_type_o    : out std_logic_vector(3 downto 0);    -- needed for execution
        dispatch_instr_subtype_o : out std_logic_vector(4 downto 0);    -- needed for execution
        
        -- stall signals
        stall_if_o              : out std_logic;
        stall_id_o              : out std_logic;
        stall_rn_o              : out std_logic;
        stall_is_o              : out std_logic;
        stall_ex_o              : out std_logic;
        stall_iq_is_o           : out std_logic;
        flush_pipe_o            : out std_logic;
        if_id_flush_o           : out std_logic;
        
        -- ****************** Status interface **************************** 
        rob_full_i              : in std_logic;
        free_list_fifo_empty_i  : in std_logic;
        iq_full_i               : in std_logic;
        rs_full_i               : in std_logic;
        misspredict_i           : in std_logic
        
        ); 
end entity;
    
architecture behavioral of control_path is

    -- Decode signals
    signal decode_rd_we_s        : std_logic;
    signal decode_rs1_used_s     : std_logic;
    signal decode_rs2_used_s     : std_logic;
   
    signal decode_instr_type_s   : std_logic_vector(3 downto 0);
    signal decode_instr_s        : std_logic_vector(DATA_WIDTH-1 downto 0);
   
    signal decode_alu_2bit_op_s  : std_logic_vector(1 downto 0);
   
    signal decode_funct3_s       : std_logic_vector(2 downto 0);
    signal decode_funct7_s       : std_logic_vector(6 downto 0);
    
    -- Rename signals
    signal rename_rd_we_s        : std_logic;
    signal rename_rs1_used_s     : std_logic;
    signal rename_rs2_used_s     : std_logic;
    
    signal rename_alu_2bit_op_s  : std_logic_vector(1 downto 0);
    signal rename_funct3_s       : std_logic_vector(2 downto 0);
    signal rename_funct7_s       : std_logic_vector(6 downto 0);
    
    signal rename_instr_type_s   : std_logic_vector(3 downto 0);
    
    signal dipatch_alu_2bit_op_s : std_logic_vector(1 downto 0);
    signal dipatch_funct3_s      : std_logic_vector(2 downto 0);
    signal dipatch_funct7_s      : std_logic_vector(6 downto 0);
                
    signal dipatch_instr_type_s  : std_logic_vector(3 downto 0);
    
    signal stall_rn_s            : std_logic;
    signal stall_is_s            : std_logic;
    signal stall_ex_s            : std_logic;
    
    signal alu_op_s              : std_logic_vector(4 downto 0);
    signal mem_subtype_s         : std_logic_vector(4 downto 0);
    signal branch_subtype_s      : std_logic_vector(4 downto 0);
    
begin

    branch_unit: entity work.branch_predictor
        generic map (
            DATA_WIDTH      => DATA_WIDTH
        )
        port map(
            fetch_intr_i    => instruction_i,
            is_jump_o       => fetch_is_jump_o,
            is_spec_o       => fetch_is_spec_o
        );
        
    --IF/ID register
    IF_ID : process (clk) is
    begin
        if rising_edge(clk) then
            if reset = '0'  or misspredict_i = '1' then 
                decode_instr_s    <= (others => '0');
            else 
                decode_instr_s    <=  instruction_i;
            end if;
        end if;
    end process;
        
    -- ID stage
    u_ctrl_dec: entity work.ctrl_dec
        port map (
            instr_i         => decode_instr_s,

            rd_we_o         => decode_rd_we_s,
            rs1_used_o      => decode_rs1_used_s,
            rs2_used_o      => decode_rs2_used_s,
            
            instr_format_o  => instr_format_o,
            instr_type_o    => decode_instr_type_s,
      
            alu_2bit_op_o   => decode_alu_2bit_op_s,
            funct3_o        => decode_funct3_s,
            funct7_o        => decode_funct7_s
        );
    
    if_id_flush_o <= '1' when decode_instr_type_s = BRANCH or decode_instr_type_s = JAL else '0';
    
    -- RN stage
    -- ID/RN register
    ID_RN : process(clk) is
    begin
        if rising_edge(clk) then
            if reset = '0'  or misspredict_i = '1' then
                rename_rd_we_s         <= '0';
                rename_rs1_used_s      <= '0';
                rename_rs2_used_s      <= '0';
                rename_alu_2bit_op_s   <= (others => '0');
                rename_funct3_s        <= (others => '0');
                rename_funct7_s        <= (others => '0');
                rename_instr_type_s    <= (others => '0');
            elsif stall_rn_s = '0' then
                rename_rd_we_s         <= decode_rd_we_s;
                rename_rs1_used_s      <= decode_rs1_used_s;
                rename_rs2_used_s      <= decode_rs2_used_s;
                rename_alu_2bit_op_s   <= decode_alu_2bit_op_s;
                rename_funct3_s        <= decode_funct3_s;
                rename_funct7_s        <= decode_funct7_s;
                rename_instr_type_s    <= decode_instr_type_s;
            end if;
        end if;
    end process;
    
    rename_rd_we_o      <= rename_rd_we_s;
    rename_rs1_used_o   <= rename_rs1_used_s;
    rename_rs2_used_o   <= rename_rs2_used_s;
    
    rename_snapshot_o   <= '1' when rename_instr_type_s = JALR or rename_instr_type_s = BRANCH else '0';
    
    -- Dispatch / Issue stage
    -- RN/IS register
    RN_IS : process(clk) is
    begin
        if rising_edge(clk) then
            if reset = '0' or misspredict_i = '1' then
                dipatch_instr_type_s    <= (others => '0');
                dipatch_alu_2bit_op_s    <= (others => '0');
                dipatch_funct3_s        <= (others => '0');
                dipatch_funct7_s        <= (others => '0');
            elsif stall_is_s = '0' then
                dipatch_instr_type_s    <= rename_instr_type_s;
                dipatch_alu_2bit_op_s   <= rename_alu_2bit_op_s;
                dipatch_funct3_s        <= rename_funct3_s;
                dipatch_funct7_s        <= rename_funct7_s;
            end if;
        end if;
    end process;
    
    alu_dec : entity work.alu_decoder(behavioral)
         port map(
             alu_2bit_op_i => dipatch_alu_2bit_op_s,
             funct3_i      => dipatch_funct3_s,
             funct7_i      => dipatch_funct7_s,
             alu_op_o      => alu_op_s);
    
    mem_subtype_s    <=  LB     when (dipatch_instr_type_s  = LOAD and dipatch_funct3_s = "000") else
                         LH     when (dipatch_instr_type_s  = LOAD and dipatch_funct3_s = "001") else
                         LW     when (dipatch_instr_type_s  = LOAD and dipatch_funct3_s = "010") else
                         LBU    when (dipatch_instr_type_s  = LOAD and dipatch_funct3_s = "100") else
                         LHU    when (dipatch_instr_type_s  = LOAD and dipatch_funct3_s = "101") else
                         SB     when (dipatch_instr_type_s = STORE and dipatch_funct3_s = "000") else
                         SH     when (dipatch_instr_type_s = STORE and dipatch_funct3_s = "001") else
                         SW     when (dipatch_instr_type_s = STORE and dipatch_funct3_s = "010") else
                         NONE_S;    
    
    branch_subtype_s <=  BEQ    when (dipatch_instr_type_s = BRANCH and dipatch_funct3_s = "000") else
                         BNE    when (dipatch_instr_type_s = BRANCH and dipatch_funct3_s = "001") else
                         BLT    when (dipatch_instr_type_s = BRANCH and dipatch_funct3_s = "100") else
                         BGE    when (dipatch_instr_type_s = BRANCH and dipatch_funct3_s = "101") else
                         BLTU   when (dipatch_instr_type_s = BRANCH and dipatch_funct3_s = "110") else
                         BGEU   when (dipatch_instr_type_s = BRANCH and dipatch_funct3_s = "111") else
                         NONE_B;
    
    -- jal jalr ?
    dispatch_instr_type_o     <= dipatch_instr_type_s;
    dispatch_instr_subtype_o <= alu_op_s         when (dipatch_instr_type_s = R_TYPE or dipatch_instr_type_s = I_TYPE or dipatch_instr_type_s = LUI or dipatch_instr_type_s = AUIPC or dipatch_instr_type_s = JAL or dipatch_instr_type_s = JALR) else
                                mem_subtype_s    when (dipatch_instr_type_s = LOAD   or dipatch_instr_type_s = STORE) else
                                branch_subtype_s when  dipatch_instr_type_s = BRANCH else
                                NONE_L;
    
    -- Generate stall signals
    stall:process (rob_full_i, free_list_fifo_empty_i, rename_rd_we_s, iq_full_i, rs_full_i) is
    begin
        stall_if_o      <= '0';
        stall_id_o      <= '0';
        stall_rn_s      <= '0';
        stall_is_s      <= '0';
        stall_ex_s      <= '0';
        stall_iq_is_o   <= '0';
        
        -- ROB is full OR (free_list_fifo is empty AND there's an rd instruction in rename stage)
        if(rob_full_i = '1' or (free_list_fifo_empty_i = '1' and rename_rd_we_s = '1')) then
            stall_if_o <= '1';
            stall_id_o <= '1';
            stall_rn_s <= '1';
        end if;
        
        -- IQ is full
        if(iq_full_i = '1') then
            stall_if_o <= '1';
            stall_id_o <= '1';
            stall_rn_s <= '1';
            stall_is_s <= '1';
        end if;          
        
        -- Reservation station(s) are full
        -- Only block the issue from IQ and ex stage
        if(rs_full_i = '1') then
            stall_ex_s    <= '1';
            stall_iq_is_o <= '1';
        end if;
        
        
    end process;
    
    stall_rn_o  <= stall_rn_s;
    stall_is_o  <= stall_is_s;
    stall_ex_o  <= stall_ex_s;
    
    flush_pc_next: process (misspredict_i) begin
        if misspredict_i = '1' then
            pc_next_sel_o <= '1';
            flush_pipe_o  <= '1';
        else
            pc_next_sel_o <= '0';
            flush_pipe_o  <= '0';
        end if;
    end process;
    
    
end architecture;


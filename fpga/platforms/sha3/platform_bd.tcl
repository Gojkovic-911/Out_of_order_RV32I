# SHA3 platform block design

# program, data: instruction and data memory files; sim_load: reload them on restart in simulation
proc create_platform_bd {bd_name program data sim_load} {
    global cfg board_clk_mhz cpu_clk_mhz gpio_base dmem_base dmem_size

    create_bd_design $bd_name

    # External ports
    set clk [create_bd_port -dir I -type clk -freq_hz [expr {$board_clk_mhz * 1000000}] clk]
    set rst [create_bd_port -dir I -type rst rst]
    set_property CONFIG.POLARITY ACTIVE_HIGH $rst
    create_bd_port -dir O -from [expr {$cfg(GPIO_WIDTH) - 1}] -to 0 gpio

    # IP cells
    set cpu [create_bd_cell -type ip -vlnv user.org:user:CPU:2.0 CPU_0]
    set_property -dict [list CONFIG.IMEM_INIT_FILE $program CONFIG.DMEM_INIT_FILE $data CONFIG.SIM_LOAD $sim_load] $cpu
    foreach pair [cfg_generics CORE_GENERICS SYNTH_GENERICS] {
        set_property CONFIG.[lindex [split $pair =] 0] [lindex [split $pair =] 1] $cpu
    }

    set clk_wiz [create_bd_cell -type ip -vlnv xilinx.com:ip:clk_wiz:6.0 clk_wiz_0]
    set_property -dict [list \
        CONFIG.PRIM_IN_FREQ               $board_clk_mhz \
        CONFIG.CLKOUT1_REQUESTED_OUT_FREQ $cpu_clk_mhz \
        CONFIG.RESET_TYPE                 ACTIVE_HIGH \
        CONFIG.RESET_PORT                 reset] $clk_wiz

    create_bd_cell -type ip -vlnv xilinx.com:ip:proc_sys_reset:5.0 proc_sys_reset_0

    set gpio [create_bd_cell -type ip -vlnv xilinx.com:ip:axi_gpio:2.0 axi_gpio_0]
    set_property -dict [list \
        CONFIG.C_GPIO_WIDTH  $cfg(GPIO_WIDTH) \
        CONFIG.C_ALL_OUTPUTS 1] $gpio

    create_bd_cell -type ip -vlnv user.org:user:SHA3:1.0 sha3_0

    # DMA simple mode, 64-bit stream and memory side, length up to the DMEM size
    set dma [create_bd_cell -type ip -vlnv xilinx.com:ip:axi_dma:7.1 axi_dma_0]
    set_property -dict [list \
        CONFIG.c_include_sg              0 \
        CONFIG.c_sg_length_width         $cfg(DMEM_ADDR_BITS) \
        CONFIG.c_addr_width              32 \
        CONFIG.c_m_axi_mm2s_data_width   64 \
        CONFIG.c_m_axis_mm2s_tdata_width 64 \
        CONFIG.c_m_axi_s2mm_data_width   64 \
        CONFIG.c_s_axis_s2mm_tdata_width 64 \
        CONFIG.c_mm2s_burst_size         16 \
        CONFIG.c_s2mm_burst_size         16] $dma

    set sc_lite [create_bd_cell -type ip -vlnv xilinx.com:ip:smartconnect:1.0 smartconnect_lite]
    set_property -dict [list CONFIG.NUM_SI 1 CONFIG.NUM_MI 3] $sc_lite
    set sc_mem [create_bd_cell -type ip -vlnv xilinx.com:ip:smartconnect:1.0 smartconnect_mem]
    set_property -dict [list CONFIG.NUM_SI 2 CONFIG.NUM_MI 1] $sc_mem

    # CPU peripherals: GPIO, SHA3 registers, DMA registers
    connect_bd_intf_net [get_bd_intf_pins CPU_0/m_axi] [get_bd_intf_pins smartconnect_lite/S00_AXI]
    connect_bd_intf_net [get_bd_intf_pins smartconnect_lite/M00_AXI] [get_bd_intf_pins axi_gpio_0/S_AXI]
    connect_bd_intf_net [get_bd_intf_pins smartconnect_lite/M01_AXI] [get_bd_intf_pins sha3_0/s_axi]
    connect_bd_intf_net [get_bd_intf_pins smartconnect_lite/M02_AXI] [get_bd_intf_pins axi_dma_0/S_AXI_LITE]
    connect_bd_net [get_bd_pins axi_gpio_0/gpio_io_o] [get_bd_ports gpio]

    # DMA reads and writes DMEM through port B; instruction port B unused
    connect_bd_intf_net [get_bd_intf_pins axi_dma_0/M_AXI_MM2S] [get_bd_intf_pins smartconnect_mem/S00_AXI]
    connect_bd_intf_net [get_bd_intf_pins axi_dma_0/M_AXI_S2MM] [get_bd_intf_pins smartconnect_mem/S01_AXI]
    connect_bd_intf_net [get_bd_intf_pins smartconnect_mem/M00_AXI] [get_bd_intf_pins CPU_0/s_axi_dmem]

    # Message stream into SHA3, digest stream back
    connect_bd_intf_net [get_bd_intf_pins axi_dma_0/M_AXIS_MM2S] [get_bd_intf_pins sha3_0/s_axis]
    connect_bd_intf_net [get_bd_intf_pins sha3_0/m_axis] [get_bd_intf_pins axi_dma_0/S_AXIS_S2MM]

    # Clock to every block
    connect_bd_net [get_bd_ports clk] [get_bd_pins clk_wiz_0/clk_in1]
    connect_bd_net [get_bd_pins clk_wiz_0/clk_out1] \
        [get_bd_pins proc_sys_reset_0/slowest_sync_clk] \
        [get_bd_pins CPU_0/clk] \
        [get_bd_pins axi_gpio_0/s_axi_aclk] \
        [get_bd_pins sha3_0/aclk] \
        [get_bd_pins axi_dma_0/s_axi_lite_aclk] \
        [get_bd_pins axi_dma_0/m_axi_mm2s_aclk] \
        [get_bd_pins axi_dma_0/m_axi_s2mm_aclk] \
        [get_bd_pins smartconnect_lite/aclk] \
        [get_bd_pins smartconnect_mem/aclk]

    # Reset after clock lock
    connect_bd_net [get_bd_ports rst] \
        [get_bd_pins clk_wiz_0/reset] \
        [get_bd_pins proc_sys_reset_0/ext_reset_in]
    connect_bd_net [get_bd_pins clk_wiz_0/locked] [get_bd_pins proc_sys_reset_0/dcm_locked]
    connect_bd_net [get_bd_pins proc_sys_reset_0/interconnect_aresetn] \
        [get_bd_pins CPU_0/s_axi_aresetn] \
        [get_bd_pins smartconnect_lite/aresetn] \
        [get_bd_pins smartconnect_mem/aresetn]
    connect_bd_net [get_bd_pins proc_sys_reset_0/peripheral_aresetn] \
        [get_bd_pins CPU_0/reset] \
        [get_bd_pins axi_gpio_0/s_axi_aresetn] \
        [get_bd_pins sha3_0/aresetn] \
        [get_bd_pins axi_dma_0/axi_resetn]

    # Address map: peripherals for the CPU, DMEM for the DMA at the CPU address
    set space [get_bd_addr_spaces CPU_0/m_axi]
    assign_bd_address -target_address_space $space -offset $gpio_base -range 64K \
        [get_bd_addr_segs axi_gpio_0/S_AXI/Reg]
    assign_bd_address -target_address_space $space -offset $cfg(SHA3_BASE) -range 64K \
        [get_bd_addr_segs sha3_0/s_axi/reg0]
    assign_bd_address -target_address_space $space -offset $cfg(DMA_BASE) -range 64K \
        [get_bd_addr_segs axi_dma_0/S_AXI_LITE/Reg]
    foreach dma_space {Data_MM2S Data_S2MM} {
        assign_bd_address -target_address_space [get_bd_addr_spaces axi_dma_0/$dma_space] \
            -offset $dmem_base -range $dmem_size [get_bd_addr_segs CPU_0/s_axi_dmem/reg0]
    }

    validate_bd_design
    save_bd_design
}

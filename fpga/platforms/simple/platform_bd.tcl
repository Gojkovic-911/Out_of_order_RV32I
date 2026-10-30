# Simple platform block design

# program, data: instruction and data memory files; sim_load: reload them on restart in simulation
proc create_platform_bd {bd_name program data sim_load} {
    global cfg root board_clk_mhz cpu_clk_mhz dmem_base dmem_size gpio_base

    create_bd_design $bd_name

    # External ports
    set clk [create_bd_port -dir I -type clk -freq_hz [expr {$board_clk_mhz * 1000000}] clk]
    set rst [create_bd_port -dir I -type rst rst]
    set_property CONFIG.POLARITY ACTIVE_HIGH $rst
    create_bd_port -dir O -from [expr {$cfg(GPIO_WIDTH) - 1}] -to 0 gpio

    # IP cells
    set cpu [create_bd_cell -type ip -vlnv user.org:user:CPU:2.0 CPU_0]
    set_property -dict [list CONFIG.INIT_FILE $program CONFIG.SIM_LOAD $sim_load] $cpu
    foreach name [concat $cfg(CORE_GENERICS) $cfg(SYNTH_GENERICS)] {
        set_property CONFIG.$name $cfg($name) $cpu
    }

    set clk_wiz [create_bd_cell -type ip -vlnv xilinx.com:ip:clk_wiz:6.0 clk_wiz_0]
    set_property -dict [list \
        CONFIG.PRIM_IN_FREQ               $board_clk_mhz \
        CONFIG.CLKOUT1_REQUESTED_OUT_FREQ $cpu_clk_mhz \
        CONFIG.RESET_TYPE                 ACTIVE_HIGH \
        CONFIG.RESET_PORT                 reset] $clk_wiz

    create_bd_cell -type ip -vlnv xilinx.com:ip:proc_sys_reset:5.0 proc_sys_reset_0

    set ic [create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_0]
    set_property -dict [list CONFIG.NUM_SI 1 CONFIG.NUM_MI 2] $ic

    # Data memory, RTL module reference
    add_files -norecurse $root/fpga/rtl/axi_lite_ram.vhd
    update_compile_order -fileset sources_1
    set dmem [create_bd_cell -type module -reference axi_lite_ram dmem_0]
    set_property -dict [list \
        CONFIG.ADDR_BITS [mem_bits $dmem_size] \
        CONFIG.INIT_FILE $data \
        CONFIG.SIM_LOAD  $sim_load] $dmem

    set gpio [create_bd_cell -type ip -vlnv xilinx.com:ip:axi_gpio:2.0 axi_gpio_0]
    set_property -dict [list \
        CONFIG.C_GPIO_WIDTH  $cfg(GPIO_WIDTH) \
        CONFIG.C_ALL_OUTPUTS 1] $gpio

    # CPU to data memory and GPIO
    connect_bd_intf_net [get_bd_intf_pins CPU_0/m_axi]                [get_bd_intf_pins axi_interconnect_0/S00_AXI]
    connect_bd_intf_net [get_bd_intf_pins axi_interconnect_0/M00_AXI] [get_bd_intf_pins dmem_0/s_axi]
    connect_bd_intf_net [get_bd_intf_pins axi_interconnect_0/M01_AXI] [get_bd_intf_pins axi_gpio_0/S_AXI]
    connect_bd_net [get_bd_pins axi_gpio_0/gpio_io_o] [get_bd_ports gpio]

    # Clock to every block
    connect_bd_net [get_bd_ports clk] [get_bd_pins clk_wiz_0/clk_in1]
    connect_bd_net [get_bd_pins clk_wiz_0/clk_out1] \
        [get_bd_pins proc_sys_reset_0/slowest_sync_clk] \
        [get_bd_pins CPU_0/clk] \
        [get_bd_pins axi_interconnect_0/ACLK] \
        [get_bd_pins axi_interconnect_0/S00_ACLK] \
        [get_bd_pins axi_interconnect_0/M00_ACLK] \
        [get_bd_pins axi_interconnect_0/M01_ACLK] \
        [get_bd_pins dmem_0/s_axi_aclk] \
        [get_bd_pins axi_gpio_0/s_axi_aclk]

    # Reset after clock lock
    connect_bd_net [get_bd_ports rst] \
        [get_bd_pins clk_wiz_0/reset] \
        [get_bd_pins proc_sys_reset_0/ext_reset_in]
    connect_bd_net [get_bd_pins clk_wiz_0/locked] [get_bd_pins proc_sys_reset_0/dcm_locked]
    connect_bd_net [get_bd_pins proc_sys_reset_0/interconnect_aresetn] \
        [get_bd_pins axi_interconnect_0/ARESETN]
    connect_bd_net [get_bd_pins proc_sys_reset_0/peripheral_aresetn] \
        [get_bd_pins CPU_0/reset] \
        [get_bd_pins axi_interconnect_0/S00_ARESETN] \
        [get_bd_pins axi_interconnect_0/M00_ARESETN] \
        [get_bd_pins axi_interconnect_0/M01_ARESETN] \
        [get_bd_pins dmem_0/s_axi_aresetn] \
        [get_bd_pins axi_gpio_0/s_axi_aresetn]

    # Address map
    set space [get_bd_addr_spaces CPU_0/m_axi]
    assign_bd_address -target_address_space $space -offset $dmem_base -range $dmem_size \
        [get_bd_addr_segs dmem_0/s_axi/*]
    assign_bd_address -target_address_space $space -offset $gpio_base -range $cfg(GPIO_SIZE) \
        [get_bd_addr_segs axi_gpio_0/S_AXI/Reg]

    validate_bd_design
    save_bd_design
}

# Address bits of a size like 16K or 1M
proc mem_bits {size} {
    set mult [dict get {K 1024 M 1048576} [string index $size end]]
    set bytes [expr {[string range $size 0 end-1] * $mult}]
    return [expr {int(round(log($bytes) / log(2)))}]
}

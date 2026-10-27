# Zybo pins of design_1_wrapper

# 125 MHz clock (L16)
set_property -dict { PACKAGE_PIN L16 IOSTANDARD LVCMOS33 } [get_ports clk]

# Reset on BTN0
set_property -dict { PACKAGE_PIN R18 IOSTANDARD LVCMOS33 } [get_ports rst]

# GPIO on LD0-LD3
set_property -dict { PACKAGE_PIN M14 IOSTANDARD LVCMOS33 } [get_ports {gpio[0]}]
set_property -dict { PACKAGE_PIN M15 IOSTANDARD LVCMOS33 } [get_ports {gpio[1]}]
set_property -dict { PACKAGE_PIN G14 IOSTANDARD LVCMOS33 } [get_ports {gpio[2]}]
set_property -dict { PACKAGE_PIN D18 IOSTANDARD LVCMOS33 } [get_ports {gpio[3]}]

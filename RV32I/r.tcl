# ======================================================
# TCL SCRIPT FOR VIVADO: runs bash scripts and relaunches simulation
# ======================================================

# 1. Run the bash script for binary conversion (or any pre-simulation setup)
puts "Running convert_bin.sh..."
exec bash convert_bin.sh

# 2. Relaunch the simulation (loads the updated design)
puts "Relaunching simulation..."
relaunch_sim
run 50 us

puts "Running reg_dump.tcl..."
source reg_dump.tcl

# 3. Run the comparison script (to verify results after simulation)
puts "Running compare_script.sh..."
exec bash compare_script.sh

puts "All scripts completed."
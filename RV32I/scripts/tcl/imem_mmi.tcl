# Instruction memory map for updatemem

# One byte lane per BRAM, listed from the lowest byte
proc write_imem_mmi {file part} {
    set brams [get_cells -hier -filter {PRIMITIVE_TYPE =~ BMEM.*.* && NAME =~ *INSTR_MEM*}]
    if {[llength $brams] == 0} {
        error "No instruction memory BRAM found"
    }
    set fh [open $file w]
    puts $fh {<?xml version="1.0" encoding="UTF-8"?>}
    puts $fh {<MemInfo Version="1" Minor="0">}
    puts $fh {  <Processor Endianness="Little" InstPath="imem">}
    puts $fh "    <AddressSpace Name=\"imem\" Begin=\"0\" End=\"16383\">"
    puts $fh {      <BusBlock>}
    foreach b [lsort -command bram_lane_order $brams] {
        if {[get_property WRITE_WIDTH_A $b] != 9 || ![regexp {_(\d+)$} [get_property NAME $b] -> lane]} {
            error "Unexpected instruction memory BRAM [get_property NAME $b]"
        }
        set loc [string map {RAMB36_ ""} [get_property LOC $b]]
        puts $fh "        <BitLane MemType=\"RAMB32\" Placement=\"$loc\">"
        puts $fh "          <DataWidth MSB=\"[expr {8 * $lane + 7}]\" LSB=\"[expr {8 * $lane}]\"/>"
        puts $fh {          <AddressRange Begin="0" End="4095"/>}
        puts $fh {          <Parity ON="false" NumBits="0"/>}
        puts $fh {        </BitLane>}
    }
    puts $fh {      </BusBlock>}
    puts $fh {    </AddressSpace>}
    puts $fh {  </Processor>}
    puts $fh "  <Config><Option Name=\"Part\" Val=\"$part\"/></Config>"
    puts $fh {</MemInfo>}
    close $fh
}

proc bram_lane_order {a b} {
    regexp {_(\d+)$} [get_property NAME $a] -> la
    regexp {_(\d+)$} [get_property NAME $b] -> lb
    return [expr {$la - $lb}]
}

# Memory map for updatemem: instruction and data memory

# One processor entry per memory: name, BRAM cell filter, base address, size in bytes
proc write_mem_mmi {file part mems} {
    set fh [open $file w]
    puts $fh {<?xml version="1.0" encoding="UTF-8"?>}
    puts $fh {<MemInfo Version="1" Minor="0">}
    foreach {name filter base bytes} $mems {
        write_mmi_processor $fh $name $filter $base $bytes
    }
    puts $fh "  <Config><Option Name=\"Part\" Val=\"$part\"/></Config>"
    puts $fh {</MemInfo>}
    close $fh
}

# One byte lane per BRAM, listed from the lowest byte
proc write_mmi_processor {fh name filter base bytes} {
    set brams [get_cells -hier -filter "PRIMITIVE_TYPE =~ BMEM.*.* && NAME =~ $filter"]
    if {[llength $brams] != 4} {
        error "Expected 4 byte-lane BRAMs for $name, found [llength $brams]"
    }
    puts $fh "  <Processor Endianness=\"Little\" InstPath=\"$name\">"
    puts $fh "    <AddressSpace Name=\"$name\" Begin=\"[expr {$base}]\" End=\"[expr {$base + $bytes - 1}]\">"
    puts $fh {      <BusBlock>}
    foreach b [lsort -command bram_lane_order $brams] {
        if {[get_property WRITE_WIDTH_A $b] != 9 || ![regexp {_(\d+)$} [get_property NAME $b] -> lane]} {
            error "Unexpected $name BRAM [get_property NAME $b]"
        }
        set loc [string map {RAMB36_ ""} [get_property LOC $b]]
        puts $fh "        <BitLane MemType=\"RAMB32\" Placement=\"$loc\">"
        puts $fh "          <DataWidth MSB=\"[expr {8 * $lane + 7}]\" LSB=\"[expr {8 * $lane}]\"/>"
        puts $fh "          <AddressRange Begin=\"0\" End=\"[expr {$bytes / 4 - 1}]\"/>"
        puts $fh {          <Parity ON="false" NumBits="0"/>}
        puts $fh {        </BitLane>}
    }
    puts $fh {      </BusBlock>}
    puts $fh {    </AddressSpace>}
    puts $fh {  </Processor>}
}

proc bram_lane_order {a b} {
    regexp {_(\d+)$} [get_property NAME $a] -> la
    regexp {_(\d+)$} [get_property NAME $b] -> lb
    return [expr {$la - $lb}]
}

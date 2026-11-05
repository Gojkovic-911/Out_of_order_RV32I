# SHA3 Platform
CPU (with IMEM and DMEM), GPIO, SHA3-256 accelerator fed by AXI DMA, clock wizard, reset

## Block design

```
CPU_0/m_axi ──> smartconnect_lite ──┬──> axi_gpio_0      (LD0-LD3)
   (AXI4-Lite)                      ├──> sha3_0/s_axi    (registers)
                                    └──> axi_dma_0/S_AXI_LITE

axi_dma_0/M_AXI_MM2S ──┐
axi_dma_0/M_AXI_S2MM ──┴──> smartconnect_mem ──> CPU_0/s_axi_dmem (DMEM port B)
   (AXI4, 64 bit)            (64 -> 32 bit)

axi_dma_0/M_AXIS_MM2S ──> sha3_0/s_axis   padded message, 64-bit lanes
sha3_0/m_axis ──> axi_dma_0/S_AXIS_S2MM   digest, 4 lanes, tlast on the last
```

- AXI DMA in simple mode (no scatter-gather), 64-bit stream and memory side, length register `DMEM_ADDR_BITS` wide
- The DMA sees DMEM at `DMEM_BASE`, the same address as the CPU, so the firmware passes plain pointers
- `CPU_0/s_axi_imem` is unused; `s_axi_aresetn` comes from `interconnect_aresetn`

## Address map

| Master | Slave | Address | Range |
|---|---|---|---|
| CPU `m_axi` | GPIO | `GPIO_BASE` | 64K |
| CPU `m_axi` | SHA3 registers | `SHA3_BASE` | 64K |
| CPU `m_axi` | DMA registers | `DMA_BASE` | 64K |
| DMA MM2S, S2MM | DMEM | `DMEM_BASE` (`cpu.mk`) | 2^`DMEM_ADDR_BITS` |

## Configuration

`sha3.mk`:

| Parameter | Default | Description |
|---|---|---|
| `CPU_CLK_MHZ` | 120 | CPU clock |
| `GPIO_BASE` | `0x40000000` | GPIO address |
| `GPIO_WIDTH` | 4 | GPIO outputs |
| `SHA3_BASE` | `0x40010000` | SHA3 register address |
| `DMA_BASE` | `0x40020000` | AXI DMA register address |
| `FPGA_IPS` | `sha3` | our IPs from `fpga/ip/`, packaged before sim and bitstream |
| `DEFAULT_FW` | `sha3` | firmware from `sw/fw/sha3/` |

## Hash sequence

Drivers in `sw/drivers/` (`axi_dma.h`, `sha3.h`):

1. `dma_init()` once: RS = 1 on both DMA channels
2. `sha3_pad()`: message, `0x06`, zeros, `0x80` in the last byte, to a multiple of 136 bytes, 8-byte aligned buffer
3. `dma_s2mm_start(digest, 32)`: S2MM ready for the digest
4. `sha3_start(nb)`: block count, start
5. `dma_mm2s_start(padded, nb * 136)`: the message into SHA3; the length must be exactly nb * 136
6. `dma_s2mm_wait()`: S2MM IOC, the digest is in DMEM

## Firmware

`sha3`: SHA3-256 of `""`, `"abc"` and 200 bytes `i & 0xff`, compared with hashlib digests. LD0-LD2 light for each passing message, LD3 when all pass.

```
make fpga sim PLATFORM=sha3                # stress tests on the platform
make fpga sim PLATFORM=sha3 FW=sha3        # firmware run, waveform
make fpga bitstream PLATFORM=sha3
make fpga fw-update PLATFORM=sha3 FW=sha3
```

## Results

| | |
|---|---|
| Simulation | stress tests 14/14 |
| SHA3 | one round per cycle, 43 cycles per extra block (~380 MB/s at 120 MHz) |
| Bitstream | WNS +0.256 ns, WHS +0.014 ns at 120 MHz |
| Resources | 10 159 LUT, 10 908 FF, 10 RAMB36 |
| Per block | CPU 3 199 LUT, SHA3 3 005, smartconnect_mem 2 029, DMA 1 485, smartconnect_lite 391 |

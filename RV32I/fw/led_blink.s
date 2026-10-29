# 4-bit counter on GPIO LD0-LD3
# About 0.66 s per step at 120 MHz
.equ DELAY, 15000000
        lui   x5, 0x40000
        addi  x7, x0, 0
loop:
        sw    x7, 0(x5)
        addi  x7, x7, 1
        li    x6, DELAY
wait:
        addi  x6, x6, -1
        bne   x6, x0, wait
        j     loop
halt:
        nop
        j     halt

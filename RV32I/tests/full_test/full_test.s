# ===================================================================
# STRESS TEST - ALU PHASES DISPATCHED THROUGH A FUNCTION TABLE
# Four ALU phases (add / xor / or-and / shift-compare chains) are
# subroutines. Their addresses go into a table in memory and a loop
# calls them through jalr on the loaded pointer; a second pass runs in
# reverse order and skips one phase on a data dependent branch.
# x1 = return address
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x5,  x0, 1
addi    x6,  x0, 2
addi    x7,  x0, 3
addi    x8,  x0, 4
addi    x9,  x0, 5
addi    x10, x0, 6
addi    x11, x0, 7
addi    x12, x0, 8
addi    x13, x0, 9
addi    x14, x0, 10
addi    x15, x0, 11
addi    x16, x0, 12
addi    x31, x0, 0            # checksum

# -------------------- FUNCTION TABLE AT 0x100 -----------------------
# jal over each phase: the link register holds the phase address
jal     x20, over_phase1
phase1:
add     x17, x5,  x6
add     x18, x17, x7
add     x19, x18, x8
add     x5,  x19, x9
add     x6,  x5,  x10
add     x7,  x6,  x11
add     x8,  x7,  x12
add     x31, x31, x8
jalr    x0,  x1,  0
over_phase1:
sw      x20, 0x100(x0)

jal     x20, over_phase2
phase2:
xor     x17, x8,  x7
xor     x18, x17, x6
xor     x19, x18, x5
xor     x9,  x19, x16
xor     x10, x9,  x15
xor     x11, x10, x14
xor     x12, x11, x13
xor     x31, x31, x12
jalr    x0,  x1,  0
over_phase2:
sw      x20, 0x104(x0)

jal     x20, over_phase3
phase3:
or      x17, x12, x5
and     x18, x17, x6
or      x19, x18, x7
and     x13, x19, x8
or      x14, x13, x9
and     x15, x14, x10
or      x16, x15, x11
add     x31, x31, x16
jalr    x0,  x1,  0
over_phase3:
sw      x20, 0x108(x0)

jal     x20, over_phase4
phase4:
sll     x17, x5,  x6
srl     x18, x17, x7
sra     x19, x31, x8
slt     x5,  x19, x18
sltu    x6,  x19, x18
sll     x7,  x16, x5
srl     x8,  x17, x6
xor     x31, x31, x19
jalr    x0,  x1,  0
over_phase4:
sw      x20, 0x10C(x0)

# -------------------- FORWARD PASS: phase1..phase4 ------------------
addi    x21, x0, 0            # table offset
addi    x22, x0, 16
fwd_loop:
lw      x23, 0x100(x21)
jalr    x1,  x23, 0           # call through the loaded pointer
addi    x21, x21, 4
blt     x21, x22, fwd_loop

# -------------------- REVERSE PASS: phase4..phase1 ------------------
# phase3 is skipped when the checksum is odd
addi    x21, x0, 12
rev_loop:
addi    x24, x0, 8
bne     x21, x24, rev_call
andi    x25, x31, 1
bne     x25, x0,  rev_next    # data dependent skip
rev_call:
lw      x23, 0x100(x21)
jalr    x1,  x23, 0
rev_next:
addi    x21, x21, -4
bge     x21, x0,  rev_loop


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# Slow head for the sections below: pointer ring at 0x2000
lui     x7,  0x2
addi    x8, x7, 64
sw      x8, 0(x7)
addi    x9, x7, 128
sw      x9, 0(x8)
addi    x10, x7, 192
sw      x10, 0(x9)
sw      x7, 0(x10)

# TARGET: ROB full - 70 independent ALU ops behind a slow head
lw      x11, 0(x7)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
add     x31, x31, x11
addi    x11, x0, 3
addi    x12, x0, 10
addi    x13, x0, 17
addi    x14, x0, 24
addi    x15, x0, 31
addi    x16, x0, 38
addi    x17, x0, 45
addi    x18, x0, 52
addi    x19, x0, 59
addi    x20, x0, 66
addi    x21, x0, 73
addi    x22, x0, 80
addi    x23, x0, 87
addi    x24, x0, 94
addi    x25, x0, 101
addi    x26, x0, 108
addi    x27, x0, 115
addi    x11, x0, 122
addi    x12, x0, 129
addi    x13, x0, 136
addi    x14, x0, 143
addi    x15, x0, 150
addi    x16, x0, 157
addi    x17, x0, 164
addi    x18, x0, 171
addi    x19, x0, 178
addi    x20, x0, 185
addi    x21, x0, 192
addi    x22, x0, 199
addi    x23, x0, 206
addi    x24, x0, 213
addi    x25, x0, 220
addi    x26, x0, 227
addi    x27, x0, 234
addi    x11, x0, 241
addi    x12, x0, 248
addi    x13, x0, 255
addi    x14, x0, 262
addi    x15, x0, 269
addi    x16, x0, 276
addi    x17, x0, 283
addi    x18, x0, 290
addi    x19, x0, 297
addi    x20, x0, 304
addi    x21, x0, 311
addi    x22, x0, 318
addi    x23, x0, 325
addi    x24, x0, 332
addi    x25, x0, 339
addi    x26, x0, 346
addi    x27, x0, 353
addi    x11, x0, 360
addi    x12, x0, 367
addi    x13, x0, 374
addi    x14, x0, 381
addi    x15, x0, 388
addi    x16, x0, 395
addi    x17, x0, 402
addi    x18, x0, 409
addi    x19, x0, 416
addi    x20, x0, 423
addi    x21, x0, 430
addi    x22, x0, 437
addi    x23, x0, 444
addi    x24, x0, 451
addi    x25, x0, 458
addi    x26, x0, 465
addi    x27, x0, 472
addi    x11, x0, 479
addi    x12, x0, 486
add     x31, x31, x11
add     x31, x31, x12
add     x31, x31, x13
add     x31, x31, x14
add     x31, x31, x15
add     x31, x31, x16
add     x31, x31, x17
add     x31, x31, x18
add     x31, x31, x19
add     x31, x31, x20
add     x31, x31, x21
add     x31, x31, x22
add     x31, x31, x23
add     x31, x31, x24
add     x31, x31, x25
add     x31, x31, x26
add     x31, x31, x27

# TARGET: IQ full + ALU RS full - 40 instructions waiting on the same
# slow tag, one broadcast wakes all of them at once
lw      x12, 0(x7)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
addi    x13, x12, 0
addi    x14, x12, 1
addi    x15, x12, 2
addi    x16, x12, 3
addi    x17, x12, 4
addi    x18, x12, 5
addi    x19, x12, 6
addi    x20, x12, 7
addi    x21, x12, 8
addi    x22, x12, 9
addi    x23, x12, 10
addi    x24, x12, 11
addi    x25, x12, 12
addi    x26, x12, 13
addi    x27, x12, 14
addi    x13, x12, 15
addi    x14, x12, 16
addi    x15, x12, 17
addi    x16, x12, 18
addi    x17, x12, 19
addi    x18, x12, 20
addi    x19, x12, 21
addi    x20, x12, 22
addi    x21, x12, 23
addi    x22, x12, 24
addi    x23, x12, 25
addi    x24, x12, 26
addi    x25, x12, 27
addi    x26, x12, 28
addi    x27, x12, 29
addi    x13, x12, 30
addi    x14, x12, 31
addi    x15, x12, 32
addi    x16, x12, 33
addi    x17, x12, 34
addi    x18, x12, 35
addi    x19, x12, 36
addi    x20, x12, 37
addi    x21, x12, 38
addi    x22, x12, 39
add     x31, x31, x13
add     x31, x31, x14
add     x31, x31, x15
add     x31, x31, x16
add     x31, x31, x17
add     x31, x31, x18
add     x31, x31, x19
add     x31, x31, x20
add     x31, x31, x21
add     x31, x31, x22
add     x31, x31, x23
add     x31, x31, x24
add     x31, x31, x25
add     x31, x31, x26
add     x31, x31, x27

# TARGET: LSQ full - 24 stores in a row behind a slow head: rename
# stalls on the LSQ while the ROB still has room
lw      x13, 0(x7)
lw      x13, 0(x13)
lw      x13, 0(x13)
lw      x13, 0(x13)
lw      x13, 0(x13)
lw      x13, 0(x13)
lw      x13, 0(x13)
lw      x13, 0(x13)
addi    x14, x7,  0x400       # 0x2400
sw      x15, 0(x14)
sw      x16, 4(x14)
sw      x17, 8(x14)
sw      x18, 12(x14)
sw      x19, 16(x14)
sw      x20, 20(x14)
sw      x21, 24(x14)
sw      x22, 28(x14)
sw      x23, 32(x14)
sw      x24, 36(x14)
sw      x15, 40(x14)
sw      x16, 44(x14)
sw      x17, 48(x14)
sw      x18, 52(x14)
sw      x19, 56(x14)
sw      x20, 60(x14)
sw      x21, 64(x14)
sw      x22, 68(x14)
sw      x23, 72(x14)
sw      x24, 76(x14)
sw      x15, 80(x14)
sw      x16, 84(x14)
sw      x17, 88(x14)
sw      x18, 92(x14)
lw      x15, 0(x14)
lw      x16, 44(x14)
lw      x17, 92(x14)
add     x31, x31, x15
add     x31, x31, x16
xor     x31, x31, x17

# TARGET: everything full at a misprediction - the late branch lets the
# wrong path fill ROB / IQ / RS / LSQ, the flush has to drop all of it
lw      x16, 0(x7)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
addi    x17, x7,  0x600       # 0x2600
blt     x16, x7,  full_wrong  # equal: not taken
lw      x18, 0(x17)           # wrong path must not have written it
add     x31, x31, x18
addi    x31, x31, 1
jal     x0,  full_done
full_wrong:
sw      x31, 0(x17)
lw      x12, 4(x17)
addi    x13, x16, 2
sw      x31, 12(x17)
lw      x15, 16(x17)
addi    x16, x19, 5
sw      x31, 24(x17)
lw      x18, 28(x17)
addi    x19, x22, 8
sw      x31, 36(x17)
lw      x21, 40(x17)
addi    x22, x25, 11
sw      x31, 48(x17)
lw      x24, 52(x17)
addi    x25, x13, 14
sw      x31, 60(x17)
lw      x12, 64(x17)
addi    x13, x16, 17
sw      x31, 72(x17)
lw      x15, 76(x17)
addi    x16, x19, 20
sw      x31, 84(x17)
lw      x18, 88(x17)
addi    x19, x22, 23
sw      x31, 96(x17)
lw      x21, 100(x17)
addi    x22, x25, 26
sw      x31, 108(x17)
lw      x24, 112(x17)
addi    x25, x13, 29
sw      x31, 120(x17)
lw      x12, 124(x17)
addi    x13, x16, 32
sw      x31, 132(x17)
lw      x15, 136(x17)
addi    x16, x19, 35
sw      x31, 144(x17)
lw      x18, 148(x17)
addi    x19, x22, 38
sw      x31, 156(x17)
lw      x21, 160(x17)
addi    x22, x25, 41
sw      x31, 168(x17)
lw      x24, 172(x17)
addi    x25, x13, 44
sw      x31, 180(x17)
lw      x12, 184(x17)
addi    x13, x16, 47
sw      x31, 192(x17)
lw      x15, 196(x17)
addi    x16, x19, 50
sw      x31, 204(x17)
lw      x18, 208(x17)
addi    x19, x22, 53
addi    x31, x0,  0x7AD
full_done:

# -------------------- RESULT ----------------------------------------
add     x30, x31, x16
lw      x26, 0x100(x0)
lw      x27, 0x104(x0)
lw      x28, 0x108(x0)
lw      x29, 0x10C(x0)

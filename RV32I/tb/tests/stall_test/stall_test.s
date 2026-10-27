# ===================================================================
# MIXED TEST - CORRECTLY PREDICTED BRANCHES
# ALU (R/I type, shifts, compares, lui/auipc) mixed with loads/stores.
# Static prediction = taken; a loop calling a subroutine (backward
# branch right on all but the last iteration, jalr return) and a final
# taken branch whose path runs speculatively (its stores wait for the
# branch to resolve). x30 = return address
# Golden interpreter supports only lw/sw/lb/sb for memory.
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x1,  x0, 0x100        # array A
addi    x2,  x0, 0x200        # array B
addi    x3,  x0, 0x300        # scratch
lui     x4,  0xC0FEF
addi    x4,  x4, -274         # 0xC0FEEEEE
addi    x5,  x0, 7
addi    x6,  x0, -3
addi    x31, x0, 0            # checksum

# -------------------- BUILD ARRAY A (16 words) ----------------------
# Dependent ALU recurrence, each result stored right away (LSQ fills)
add     x7,  x4,  x5
sw      x7,  0(x1)
xor     x8,  x7,  x6
sw      x8,  4(x1)
sll     x9,  x8,  x5
sw      x9,  8(x1)
sra     x10, x9,  x5
sw      x10, 12(x1)
sub     x11, x10, x4
sw      x11, 16(x1)
srl     x12, x11, x5
sw      x12, 20(x1)
or      x13, x12, x6
sw      x13, 24(x1)
and     x14, x13, x4
sw      x14, 28(x1)
sltu    x15, x14, x13
sw      x15, 32(x1)
slt     x16, x13, x14
sw      x16, 36(x1)
addi    x17, x14, -2000
sw      x17, 40(x1)
xori    x18, x17, 0x5A5
sw      x18, 44(x1)
slli    x19, x18, 3
sw      x19, 48(x1)
srai    x20, x19, 5
sw      x20, 52(x1)
srli    x21, x20, 9
sw      x21, 56(x1)
andi    x22, x21, 0x3F0
sw      x22, 60(x1)

# -------------------- STREAM: B[i] = f(A[i], A[i+1]) ----------------
lw      x7,  0(x1)
lw      x8,  4(x1)
add     x9,  x7,  x8
sw      x9,  0(x2)
lw      x10, 8(x1)
add     x11, x8,  x10
sw      x11, 4(x2)
lw      x12, 12(x1)
sub     x13, x10, x12
sw      x13, 8(x2)
lw      x14, 16(x1)
xor     x15, x12, x14
sw      x15, 12(x2)
lw      x16, 20(x1)
or      x17, x14, x16
sw      x17, 16(x2)
lw      x18, 24(x1)
and     x19, x16, x18
sw      x19, 20(x2)
# read B right after the stores (store -> load through memory)
lw      x20, 0(x2)
lw      x21, 4(x2)
lw      x22, 8(x2)
lw      x23, 12(x2)
lw      x24, 16(x2)
lw      x25, 20(x2)
add     x31, x31, x20
add     x31, x31, x21
xor     x31, x31, x22
add     x31, x31, x23
xor     x31, x31, x24
add     x31, x31, x25

# -------------------- BYTE PACKING ----------------------------------
sw      x0,  0(x3)
sb      x7,  0(x3)
sb      x9,  1(x3)
sb      x13, 2(x3)
sb      x17, 3(x3)
lw      x26, 0(x3)
lb      x27, 1(x3)
lb      x28, 3(x3)
add     x31, x31, x26
add     x31, x31, x27
add     x31, x31, x28

# -------------------- ADDRESSES FROM MEMORY -------------------------
addi    x7,  x0, 0x320
sw      x2,  0(x7)            # [0x320] = &B
sw      x1,  4(x7)            # [0x324] = &A
lw      x8,  0(x7)            # &B
lw      x9,  4(x7)            # &A
lw      x10, 8(x8)            # B[2]
lw      x11, 60(x9)           # A[15]
sw      x10, 60(x9)           # swap them through memory
sw      x11, 8(x8)
add     x31, x31, x10
xor     x31, x31, x11

# -------------------- LOOP WITH A CALL ------------------------------
# D[i] = scale(A[i]) for i = 0..7
addi    x20, x0, 0
addi    x21, x0, 32
scale_loop:
lw      x10, 0x100(x20)
jal     x30, scale
sw      x10, 0x360(x20)
addi    x20, x20, 4
blt     x20, x21, scale_loop
lw      x22, 0x36C(x0)
add     x31, x31, x22
jal     x0,  after_scale
# x10 = (x10 >> 2) + (x10 & 0xFF)
scale:
srai    x23, x10, 2
andi    x24, x10, 0xFF
add     x10, x23, x24
jalr    x0,  x30, 0
after_scale:


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: correct branch resolving while rename is stalled on a full
# LSQ - the taken path is 30 stores (spec clear timing broke here before)
lui     x7,  0x2
addi    x8, x7, 64
sw      x8, 0(x7)
addi    x9, x7, 128
sw      x9, 0(x8)
addi    x10, x7, 192
sw      x10, 0(x9)
sw      x7, 0(x10)
lw      x11, 0(x7)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
addi    x16, x7,  1024
addi    x16, x16, 1280       # 0x2900
beq     x11, x7,  lsq_taken   # equal: taken
addi    x31, x0,  0x7AD
lsq_taken:
sw      x9, 0(x16)
sw      x10, 4(x16)
sw      x11, 8(x16)
sw      x12, 12(x16)
sw      x13, 16(x16)
sw      x14, 20(x16)
sw      x9, 24(x16)
sw      x10, 28(x16)
sw      x11, 32(x16)
sw      x12, 36(x16)
sw      x13, 40(x16)
sw      x14, 44(x16)
sw      x9, 48(x16)
sw      x10, 52(x16)
sw      x11, 56(x16)
sw      x12, 60(x16)
sw      x13, 64(x16)
sw      x14, 68(x16)
sw      x9, 72(x16)
sw      x10, 76(x16)
sw      x11, 80(x16)
sw      x12, 84(x16)
sw      x13, 88(x16)
sw      x14, 92(x16)
sw      x9, 96(x16)
sw      x10, 100(x16)
sw      x11, 104(x16)
sw      x12, 108(x16)
sw      x13, 112(x16)
sw      x14, 116(x16)
lw      x17, 0(x16)
lw      x18, 116(x16)
add     x31, x31, x17
add     x31, x31, x18

# TARGET: correct branch resolving while the ROB is full (70 ops behind
# the slow head on the taken path)
lw      x11, 0(x7)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
lw      x11, 0(x11)
bge     x11, x7,  rob_taken   # equal: taken
addi    x31, x0,  0x7AE
rob_taken:
addi    x9,  x0, 5
addi    x10, x0, 12
addi    x11, x0, 19
addi    x12, x0, 26
addi    x13, x0, 33
addi    x14, x0, 40
addi    x15, x0, 47
addi    x16, x0, 54
addi    x17, x0, 61
addi    x18, x0, 68
addi    x19, x0, 75
addi    x20, x0, 82
addi    x21, x0, 89
addi    x22, x0, 96
addi    x23, x0, 103
addi    x24, x0, 110
addi    x25, x0, 117
addi    x26, x0, 124
addi    x27, x0, 131
addi    x28, x0, 138
addi    x9,  x0, 145
addi    x10, x0, 152
addi    x11, x0, 159
addi    x12, x0, 166
addi    x13, x0, 173
addi    x14, x0, 180
addi    x15, x0, 187
addi    x16, x0, 194
addi    x17, x0, 201
addi    x18, x0, 208
addi    x19, x0, 215
addi    x20, x0, 222
addi    x21, x0, 229
addi    x22, x0, 236
addi    x23, x0, 243
addi    x24, x0, 250
addi    x25, x0, 257
addi    x26, x0, 264
addi    x27, x0, 271
addi    x28, x0, 278
addi    x9,  x0, 285
addi    x10, x0, 292
addi    x11, x0, 299
addi    x12, x0, 306
addi    x13, x0, 313
addi    x14, x0, 320
addi    x15, x0, 327
addi    x16, x0, 334
addi    x17, x0, 341
addi    x18, x0, 348
addi    x19, x0, 355
addi    x20, x0, 362
addi    x21, x0, 369
addi    x22, x0, 376
addi    x23, x0, 383
addi    x24, x0, 390
addi    x25, x0, 397
addi    x26, x0, 404
addi    x27, x0, 411
addi    x28, x0, 418
addi    x9,  x0, 425
addi    x10, x0, 432
addi    x11, x0, 439
addi    x12, x0, 446
addi    x13, x0, 453
addi    x14, x0, 460
addi    x15, x0, 467
addi    x16, x0, 474
addi    x17, x0, 481
addi    x18, x0, 488
add     x31, x31, x9
add     x31, x31, x10
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
add     x31, x31, x28

# TARGET: correctly predicted branches inside a loop with a store every
# iteration (30 iterations, loop branch and if-branch both taken)
addi    x9,  x0,  30
addi    x10, x7,  1024
addi    x10, x10, 1536       # 0x2A00
corr_loop:
andi    x11, x9,  1
bge     x11, x0,  corr_store  # always taken
addi    x31, x0,  0x7AF
corr_store:
sw      x9,  0(x10)
addi    x10, x10, 4
addi    x9,  x9,  -1
bne     x9,  x0,  corr_loop
lw      x12, -4(x10)
lw      x13, -120(x10)
add     x31, x31, x12
add     x31, x31, x13

# -------------------- BRANCH (taken, predicted taken) ---------------
# Condition comes from a load, so the branch resolves late
lw      x12, 32(x1)           # A[8] (sltu result: 0 or 1)
addi    x13, x12, 5
blt     x12, x13, taken_path  # always true

# ========== FALL-THROUGH (never executed) ===========================
sw      x4,  0(x1)
sw      x4,  0(x2)
addi    x31, x0, 0x7AD
jal     x0,  readback

# ========== TAKEN PATH (speculative until the branch resolves) ======
taken_path:
lw      x14, 0(x1)
lw      x15, 4(x1)
sw      x15, 0(x1)            # swap A[0] and A[1]
sw      x14, 4(x1)
addi    x16, x0, 0x340
sw      x31, 0(x16)           # checksum to memory
sb      x5,  4(x16)
sb      x6,  5(x16)
lw      x17, 0(x16)
lb      x18, 5(x16)
add     x19, x17, x18
sw      x19, 8(x16)
auipc   x20, 0
sw      x20, 12(x16)
jal     x21, after_jal        # x21 = return address
addi    x31, x0, 0x7AD        # skipped
after_jal:
sw      x21, 16(x16)
slti    x22, x6,  0
sltiu   x23, x6,  5
or      x24, x22, x23
sw      x24, 20(x16)

# ========== READBACK OF THE FINAL MEMORY STATE ======================
readback:
lw      x1,  0x100(x0)
lw      x2,  0x104(x0)
lw      x3,  0x108(x0)
lw      x4,  0x10C(x0)
lw      x5,  0x110(x0)
lw      x6,  0x114(x0)
lw      x7,  0x118(x0)
lw      x8,  0x11C(x0)
lw      x9,  0x120(x0)
lw      x10, 0x124(x0)
lw      x11, 0x128(x0)
lw      x12, 0x12C(x0)
lw      x13, 0x130(x0)
lw      x14, 0x134(x0)
lw      x15, 0x138(x0)
lw      x16, 0x13C(x0)
lw      x17, 0x200(x0)
lw      x18, 0x204(x0)
lw      x19, 0x208(x0)
lw      x20, 0x20C(x0)
lw      x21, 0x210(x0)
lw      x22, 0x214(x0)
lw      x23, 0x300(x0)
lw      x24, 0x320(x0)
lw      x25, 0x340(x0)
lw      x26, 0x344(x0)
lw      x27, 0x348(x0)
lw      x28, 0x34C(x0)
lw      x29, 0x350(x0)
lw      x30, 0x37C(x0)

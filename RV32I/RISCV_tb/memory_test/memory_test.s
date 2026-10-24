nop
# ===================================================================
# LSU STRESS TEST
# - word/byte stores and loads, sign extension
#   (golden interpreter supports only lw/sw/lb/sb)
# - store -> load through memory, overwrites, partial overwrites
# - store address/data produced late by long ALU chains
# - load -> load dependency (pointer chasing)
# - LSQ pressure (long run of ready loads)
# - store/load loops with a data dependent branch per element
# - memcpy subroutine called twice (jal call, jalr return)
# - stores/loads on a mispredicted path (must be flushed)
# Static prediction = taken. x30 = return address
# Data region: 0x000 - 0x3FF, x31 = checksum of loaded values
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x1,  x0, 0x100        # base A
addi    x2,  x0, 0x200        # base B
addi    x3,  x0, 0x300        # base C
lui     x4,  0xDEADC
addi    x4,  x4, -273         # 0xDEADBEEF
lui     x5,  0x12345
addi    x5,  x5, 0x678        # 0x12345678
addi    x6,  x0, -1           # 0xFFFFFFFF
addi    x7,  x0, 0x7F
addi    x8,  x0, 0x80
addi    x31, x0, 0            # checksum

# -------------------- WORD STORE -> LOAD (same address) -------------
sw      x4,  0(x1)
lw      x9,  0(x1)            # 0xDEADBEEF
sw      x5,  4(x1)
lw      x10, 4(x1)            # 0x12345678
sw      x9,  8(x1)            # store a loaded value
lw      x11, 8(x1)            # 0xDEADBEEF
add     x31, x31, x9
add     x31, x31, x10
add     x31, x31, x11

# -------------------- OVERWRITE (WAW in memory) ---------------------
sw      x4,  12(x1)
sw      x5,  12(x1)
sw      x6,  12(x1)
lw      x12, 12(x1)           # 0xFFFFFFFF
xor     x31, x31, x12

# -------------------- ADDRESS 0 (x0 base) ---------------------------
sw      x5,  0(x0)
lw      x13, 0(x0)            # 0x12345678
sw      x4,  0(x0)
lw      x14, 0(x0)            # 0xDEADBEEF
add     x31, x31, x13
add     x31, x31, x14

# -------------------- BYTE STORES / LOADS ---------------------------
sw      x0,  0(x2)            # clear word
sb      x8,  0(x2)            # byte0 = 0x80
sb      x7,  1(x2)            # byte1 = 0x7F
sb      x6,  2(x2)            # byte2 = 0xFF
sb      x5,  3(x2)            # byte3 = 0x78
lw      x15, 0(x2)            # 0x78FF7F80
lb      x16, 0(x2)            # 0xFFFFFF80
lb      x17, 1(x2)            # 0x0000007F
lb      x18, 1(x2)            # 0x0000007F
lb      x19, 2(x2)            # 0xFFFFFFFF
lb      x20, 3(x2)            # 0x00000078
add     x31, x31, x15
add     x31, x31, x16
add     x31, x31, x17
add     x31, x31, x18
add     x31, x31, x19
add     x31, x31, x20

# -------------------- BYTES INTO A WORD ----------------------------
sw      x5,  4(x2)            # 0x12345678
sb      x4,  4(x2)            # 0x123456EF
sb      x8,  7(x2)            # 0x803456EF
lw      x21, 4(x2)            # 0x803456EF
lb      x22, 4(x2)            # 0xFFFFFFEF
lb      x23, 5(x2)            # 0x00000056
lb      x24, 6(x2)            # 0x00000034
lb      x25, 7(x2)            # 0xFFFFFF80
add     x31, x31, x21
add     x31, x31, x22
add     x31, x31, x23
add     x31, x31, x24
add     x31, x31, x25

# -------------------- PARTIAL OVERWRITE OF A WORD -------------------
sw      x4,  8(x2)            # 0xDEADBEEF
sb      x0,  9(x2)            # 0xDEAD00EF
sb      x7,  10(x2)           # 0xDE7F00EF
sb      x0,  11(x2)           # 0x007F00EF
lw      x26, 8(x2)            # 0x007F00EF
lb      x27, 8(x2)            # 0xFFFFFFEF
lb      x28, 10(x2)           # 0x0000007F
add     x31, x31, x26
add     x31, x31, x27
add     x31, x31, x28

# -------------------- NEGATIVE OFFSETS ------------------------------
sw      x5,  -4(x3)           # 0x2FC
sw      x4,  -8(x3)           # 0x2F8
sb      x7,  -5(x3)           # 0x2FB
lw      x9,  -4(x3)           # 0x12345678
lw      x10, -8(x3)           # 0x7FADBEEF
add     x31, x31, x9
add     x31, x31, x10

# -------------------- STORE WITH LATE ADDRESS -----------------------
# Store address comes from a long chain, the younger load has its
# address ready right away but must still see the stored value.
addi    x11, x0, 0x30
add     x11, x11, x11         # 0x060
xor     x11, x11, x0
add     x11, x11, x11         # 0x0C0
sub     x11, x11, x0
add     x11, x11, x11         # 0x180
or      x11, x11, x0
add     x11, x11, x11         # 0x300
sw      x4,  0(x11)           # mem[0x300] = 0xDEADBEEF
lw      x12, 0(x3)            # 0xDEADBEEF
add     x31, x31, x12

# -------------------- STORE WITH LATE DATA --------------------------
addi    x13, x0, 3
add     x13, x13, x13         # 6
add     x13, x13, x13         # 12
add     x13, x13, x13         # 24
xor     x13, x13, x7          # 24 ^ 0x7F = 0x67
sub     x13, x13, x8          # 0x67 - 0x80 = 0xFFFFFFE7
sw      x13, 4(x3)
lw      x14, 4(x3)            # 0xFFFFFFE7
lb      x15, 4(x3)            # 0xFFFFFFE7
add     x31, x31, x14
add     x31, x31, x15

# -------------------- LOAD -> LOAD (pointer chasing) ----------------
addi    x16, x0, 0x380
addi    x17, x0, 0x390
addi    x18, x0, 0x3A0
addi    x19, x0, 0x3B0
sw      x17, 0(x16)           # 0x380 -> 0x390
sw      x18, 0(x17)           # 0x390 -> 0x3A0
sw      x19, 0(x18)           # 0x3A0 -> 0x3B0
sw      x5,  0(x19)           # 0x3B0 -> 0x12345678
lw      x20, 0(x16)           # 0x390
lw      x20, 0(x20)           # 0x3A0
lw      x20, 0(x20)           # 0x3B0
lw      x20, 0(x20)           # 0x12345678
add     x31, x31, x20

# -------------------- LSU RS PRESSURE -------------------------------
# Fill a table, then a long dependent chain followed by a run of
# loads whose addresses are ready immediately.
sw      x1,  0x40(x0)
sw      x2,  0x44(x0)
sw      x3,  0x48(x0)
sw      x4,  0x4C(x0)
sw      x5,  0x50(x0)
sw      x6,  0x54(x0)
sw      x7,  0x58(x0)
sw      x8,  0x5C(x0)
addi    x21, x0, 1
add     x21, x21, x21
add     x21, x21, x21
add     x21, x21, x21
add     x21, x21, x21
add     x21, x21, x21
add     x21, x21, x21
add     x21, x21, x21
add     x21, x21, x21         # 256
lw      x22, 0x40(x0)
lw      x23, 0x44(x0)
lw      x24, 0x48(x0)
lw      x25, 0x4C(x0)
lw      x26, 0x50(x0)
lw      x27, 0x54(x0)
lw      x28, 0x58(x0)
lw      x29, 0x5C(x0)
lw      x9,  0x40(x0)
lw      x10, 0x44(x0)
lw      x11, 0x48(x0)
lw      x12, 0x4C(x0)
lw      x13, 0x50(x0)
lw      x14, 0x54(x0)
lw      x15, 0x58(x0)
lw      x16, 0x5C(x0)
lb      x17, 0x40(x0)
lb      x18, 0x45(x0)
lb      x19, 0x4C(x0)
lb      x20, 0x4F(x0)
add     x31, x31, x21
add     x31, x31, x22
add     x31, x31, x23
add     x31, x31, x24
add     x31, x31, x25
add     x31, x31, x26
add     x31, x31, x27
add     x31, x31, x28
add     x31, x31, x29
xor     x31, x31, x9
xor     x31, x31, x10
xor     x31, x31, x11
xor     x31, x31, x12
xor     x31, x31, x13
xor     x31, x31, x14
xor     x31, x31, x15
xor     x31, x31, x16
add     x31, x31, x17
add     x31, x31, x18
add     x31, x31, x19
add     x31, x31, x20

# -------------------- STORE / LOAD LOOPS ----------------------------
# Fill 0x140..0x17F in a loop, then sum it back with a data
# dependent branch on every loaded value
addi    x21, x0, 0x140
addi    x22, x0, 0x180
addi    x23, x0, 3
fill_loop:
sw      x23, 0(x21)
sb      x21, 1(x21)           # byte1 = low byte of the address
slli    x23, x23, 1
xori    x23, x23, 0x15
addi    x21, x21, 4
blt     x21, x22, fill_loop
addi    x21, x0, 0x140
addi    x24, x0, 0
sum_loop:
lw      x25, 0(x21)
andi    x26, x25, 2
beq     x26, x0,  sum_skip
add     x24, x24, x25
sum_skip:
addi    x21, x21, 4
bne     x21, x22, sum_loop
add     x31, x31, x24

# -------------------- MEMCPY SUBROUTINE (called twice) --------------
# copy x12 words from x10 to x11, x30 = return address
addi    x10, x0, 0x140
addi    x11, x0, 0x1C0
addi    x12, x0, 8
jal     x30, memcpy
addi    x10, x0, 0x100
addi    x11, x0, 0x1E0
addi    x12, x0, 4
jal     x30, memcpy
lw      x27, 0x1C4(x0)
lw      x28, 0x1EC(x0)
add     x31, x31, x27
add     x31, x31, x28
jal     x0,  after_memcpy
memcpy:
lw      x13, 0(x10)
sw      x13, 0(x11)
addi    x10, x10, 4
addi    x11, x11, 4
addi    x12, x12, -1
bne     x12, x0,  memcpy
jalr    x0,  x30, 0
after_memcpy:


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: LSQ index wrap-around - 40 iterations of store + load
lui     x9,  0x1              # 0x1000
addi    x10, x0,  40
addi    x11, x0,  0
wrap_loop:
sw      x10, 0(x9)
lw      x12, 0(x9)
add     x11, x11, x12
sb      x10, 1(x9)
lb      x13, 1(x9)
add     x11, x11, x13
addi    x9,  x9,  4
addi    x10, x10, -1
bne     x10, x0,  wrap_loop
add     x31, x31, x11

# TARGET: overlapping forward copy (dst = src + 4): every load must see
# the store of the previous iteration (the result smears word 0)
lui     x14, 0x1
addi    x14, x14, 0x400       # 0x1400
addi    x15, x0,  0x111
sw      x15, 0(x14)
addi    x15, x0,  0x222
sw      x15, 4(x14)
addi    x15, x0,  0x333
sw      x15, 8(x14)
addi    x16, x0,  12
smear_loop:
lw      x17, 0(x14)
sw      x17, 4(x14)
addi    x14, x14, 4
addi    x16, x16, -1
bne     x16, x0,  smear_loop
lw      x18, 0(x14)           # 0x111
add     x31, x31, x18

# TARGET: byte merge - four sb into one word with byte and word loads
# interleaved, then an sb right after an lw of the same word
lui     x9,  0x1
addi    x9,  x9,  0x500       # 0x1500
sw      x0,  0(x9)
addi    x10, x0,  0x81
sb      x10, 0(x9)
lw      x11, 0(x9)
sb      x10, 3(x9)
lb      x12, 3(x9)
addi    x10, x0,  0x7E
sb      x10, 1(x9)
lw      x13, 0(x9)
sb      x10, 2(x9)
lw      x14, 0(x9)
sb      x0,  0(x9)
lw      x15, 0(x9)
add     x31, x31, x11
add     x31, x31, x12
add     x31, x31, x13
xor     x31, x31, x14
add     x31, x31, x15

# TARGET: aliasing - the same word through different base/offset pairs
lui     x9,  0x1
addi    x9,  x9,  0x600       # 0x1600
addi    x10, x9,  -16
addi    x11, x9,  32
addi    x12, x0,  0x5A5
sw      x12, 0(x9)
lw      x13, 16(x10)
sw      x13, -32(x11)
addi    x14, x13, 1
sw      x14, 16(x10)
lw      x15, -32(x11)
add     x31, x31, x15

# TARGET: store data from a load, 16 deep (lw -> sw -> lw ...)
lui     x9,  0x1
addi    x9,  x9,  0x700       # 0x1700
addi    x10, x0,  0x3C
sw      x10, 0(x9)
lw      x11, 0(x9)
addi    x11, x11, 0
sw      x11, 4(x9)
lw      x11, 4(x9)
addi    x11, x11, 1
sw      x11, 8(x9)
lw      x11, 8(x9)
addi    x11, x11, 2
sw      x11, 12(x9)
lw      x11, 12(x9)
addi    x11, x11, 3
sw      x11, 16(x9)
lw      x11, 16(x9)
addi    x11, x11, 4
sw      x11, 20(x9)
lw      x11, 20(x9)
addi    x11, x11, 5
sw      x11, 24(x9)
lw      x11, 24(x9)
addi    x11, x11, 6
sw      x11, 28(x9)
lw      x11, 28(x9)
addi    x11, x11, 7
sw      x11, 32(x9)
lw      x11, 32(x9)
addi    x11, x11, 8
sw      x11, 36(x9)
lw      x11, 36(x9)
addi    x11, x11, 9
sw      x11, 40(x9)
lw      x11, 40(x9)
addi    x11, x11, 10
sw      x11, 44(x9)
lw      x11, 44(x9)
addi    x11, x11, 11
sw      x11, 48(x9)
lw      x11, 48(x9)
addi    x11, x11, 12
sw      x11, 52(x9)
lw      x11, 52(x9)
addi    x11, x11, 13
sw      x11, 56(x9)
lw      x11, 56(x9)
addi    x11, x11, 14
sw      x11, 60(x9)
lw      x11, 60(x9)
addi    x11, x11, 15
sw      x11, 64(x9)
lw      x12, 64(x9)
add     x31, x31, x12

# TARGET: LSQ full - 30 stores in a row behind a slow head: rename
# stalls on the LSQ while the ROB still has room
lui     x7,  0x2
addi    x9, x7, 64
sw      x9, 0(x7)
addi    x10, x7, 128
sw      x10, 0(x9)
addi    x11, x7, 192
sw      x11, 0(x10)
sw      x7, 0(x11)
lw      x12, 0(x7)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
lw      x12, 0(x12)
addi    x19, x7,  1024
addi    x19, x19, 1024       # 0x2800
sw      x20, 0(x19)
sw      x21, 4(x19)
sw      x22, 8(x19)
sw      x23, 12(x19)
sw      x24, 16(x19)
sw      x25, 20(x19)
sw      x26, 24(x19)
sw      x27, 28(x19)
sw      x20, 32(x19)
sw      x21, 36(x19)
sw      x22, 40(x19)
sw      x23, 44(x19)
sw      x24, 48(x19)
sw      x25, 52(x19)
sw      x26, 56(x19)
sw      x27, 60(x19)
sw      x20, 64(x19)
sw      x21, 68(x19)
sw      x22, 72(x19)
sw      x23, 76(x19)
sw      x24, 80(x19)
sw      x25, 84(x19)
sw      x26, 88(x19)
sw      x27, 92(x19)
sw      x20, 96(x19)
sw      x21, 100(x19)
sw      x22, 104(x19)
sw      x23, 108(x19)
sw      x24, 112(x19)
sw      x25, 116(x19)
lw      x13, 0(x19)
lw      x14, 116(x19)
add     x31, x31, x13
add     x31, x31, x14

# TARGET: top of the bigger memory (address bits above 2 KB)
lui     x15, 0x4
addi    x15, x15, -4          # 0x3FFC (last word)
addi    x16, x0,  -1
sw      x16, 0(x15)
sb      x0,  3(x15)
lw      x17, 0(x15)
lui     x18, 0x3              # 0x3000
sw      x17, 0(x18)
lw      x19, 0(x18)
lb      x20, 2(x15)
add     x31, x31, x17
add     x31, x31, x19
add     x31, x31, x20

# -------------------- MISPREDICTED BRANCH ---------------------------
# 100 < 50 is false, but static prediction = taken:
# the wrong_path block is fetched speculatively and must be flushed.
addi    x9,  x0, 100
addi    x10, x0, 50
blt     x9,  x10, wrong_path

# ========== FALL-THROUGH PATH (executed) ============================
lw      x11, 0(x1)            # 0xDEADBEEF, wrong path must not clobber
lw      x12, 0(x2)            # 0x78FF7F80
lb      x13, 0(x0)            # 0xFFFFFFEF
sw      x8,  0x60(x0)         # store after the flush
lw      x14, 0x60(x0)         # 0x80
add     x31, x31, x11
add     x31, x31, x12
add     x31, x31, x13
add     x31, x31, x14
jal     x0,  readback

# ========== WRONG PATH (flushed - must NOT execute) =================
wrong_path:
sw      x6,  0(x1)
sw      x6,  0(x2)
sb      x6,  0(x0)
sb      x6,  0x60(x0)
sw      x6,  0x40(x0)
lw      x11, 0(x1)
lw      x12, 0(x2)
addi    x31, x0, 0x7AD
sw      x31, 0(x3)

# ========== READBACK OF THE FINAL MEMORY STATE ======================
readback:
lw      x1,  0x100(x0)
lw      x2,  0x104(x0)
lw      x3,  0x108(x0)
lw      x4,  0x10C(x0)
lw      x5,  0x000(x0)
lw      x6,  0x200(x0)
lw      x7,  0x204(x0)
lw      x8,  0x208(x0)
lw      x9,  0x2FC(x0)
lw      x10, 0x2F8(x0)
lw      x11, 0x300(x0)
lw      x12, 0x304(x0)
lw      x13, 0x380(x0)
lw      x14, 0x390(x0)
lw      x15, 0x3A0(x0)
lw      x16, 0x3B0(x0)
lw      x17, 0x40(x0)
lw      x18, 0x44(x0)
lw      x19, 0x48(x0)
lw      x20, 0x4C(x0)
lw      x21, 0x50(x0)
lw      x22, 0x54(x0)
lw      x23, 0x58(x0)
lw      x24, 0x5C(x0)
lw      x25, 0x60(x0)
lb      x26, 0x203(x0)
lb      x27, 0x207(x0)
lb      x28, 0x20A(x0)
lb      x29, 0x2FB(x0)
lw      x30, 0x1D8(x0)        # copied by memcpy

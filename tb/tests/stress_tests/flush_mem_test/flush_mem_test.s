# ===================================================================
# MIXED TEST - MISPREDICTED BRANCHES
# The first branch condition comes from a pointer chase through
# memory, so it resolves late and the wrong (predicted taken) path
# runs long: more loads/stores than the LSQ holds, stores to locations
# the correct path reads afterwards, writes to the checksum register.
# A second mispredict follows, then a subroutine whose loop branch
# mispredicts on negative elements.
# Static prediction = taken. x30 = return address
# Golden interpreter supports only lw/sw/lb/sb for memory.
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x1,  x0, 0x100        # linked list
addi    x2,  x0, 0x180        # table
addi    x3,  x0, 0x200        # results
lui     x4,  0x89ABD
addi    x4,  x4, -529         # 0x89ABCDEF
addi    x5,  x0, 100
addi    x6,  x0, 50
addi    x31, x0, 0            # checksum

# -------------------- LINKED LIST -----------------------------------
# 0x100 -> 0x110 -> 0x120 -> 0x130 -> payload 100
addi    x7,  x0, 0x110
addi    x8,  x0, 0x120
addi    x9,  x0, 0x130
sw      x7,  0(x1)
sw      x8,  0(x7)
sw      x9,  0(x8)
sw      x5,  0(x9)

# -------------------- TABLE (8 words) -------------------------------
srli    x10, x4,  4
sw      x10, 0(x2)
srai    x11, x4,  8
sw      x11, 4(x2)
slli    x12, x4,  12
sw      x12, 8(x2)
xor     x13, x10, x11
sw      x13, 12(x2)
and     x14, x12, x13
sw      x14, 16(x2)
sltu    x15, x4,  x5
sw      x15, 20(x2)
slt     x16, x4,  x5
sw      x16, 24(x2)
sub     x17, x5,  x4
sw      x17, 28(x2)

# -------------------- LOAD/ALU MIX ----------------------------------
lw      x18, 0(x2)
lw      x19, 4(x2)
add     x20, x18, x19
lw      x21, 8(x2)
sub     x22, x20, x21
lb      x23, 13(x2)
add     x24, x22, x23
sw      x24, 32(x2)
lw      x25, 32(x2)
xor     x31, x31, x25
add     x31, x31, x20


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: store committing right before a mispredicted branch commits -
# the write must happen although the flush follows right after
addi    x7,  x0,  0x111
sw      x7,  0x3C0(x0)
beq     x7,  x0,  mm_wrong1   # not taken, right behind the store
lw      x8,  0x3C0(x0)
add     x31, x31, x8
jal     x0,  mm_part2
mm_wrong1:
sw      x0,  0x3C0(x0)
addi    x31, x0,  0x7AD

# TARGET: wrong path fills the LSQ (rename stalls on LSQ full while
# speculating) and touches what the correct path reads afterwards
mm_part2:
lui     x9,  0x2
addi    x10, x9, 64
sw      x10, 0(x9)
addi    x11, x9, 128
sw      x11, 0(x10)
addi    x13, x9, 192
sw      x13, 0(x11)
sw      x9, 0(x13)
addi    x12, x9,  1024
addi    x12, x12, 1792       # 0x2B00
addi    x14, x0,  0x2C
sw      x14, 0(x12)
lw      x15, 0(x9)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
bne     x15, x9,  mm_wrong2   # equal: not taken
lw      x16, 0(x12)
add     x31, x31, x16
jal     x0,  mm_part3
mm_wrong2:
sw      x0,  0(x12)
lw      x14, 4(x12)
sw      x0,  8(x12)
lw      x16, 12(x12)
sw      x0,  16(x12)
lw      x18, 20(x12)
sw      x0,  24(x12)
lw      x20, 28(x12)
sw      x0,  32(x12)
lw      x22, 36(x12)
sw      x0,  40(x12)
lw      x14, 44(x12)
sw      x0,  48(x12)
lw      x16, 52(x12)
sw      x0,  56(x12)
lw      x18, 60(x12)
sw      x0,  64(x12)
lw      x20, 68(x12)
sw      x0,  72(x12)
lw      x22, 76(x12)
sw      x0,  80(x12)
lw      x14, 84(x12)
sw      x0,  88(x12)
lw      x16, 92(x12)
sw      x0,  96(x12)
lw      x18, 100(x12)
sw      x0,  104(x12)
lw      x20, 108(x12)
sw      x0,  112(x12)
lw      x22, 116(x12)
sw      x0,  120(x12)
lw      x14, 124(x12)
sw      x0,  128(x12)
lw      x16, 132(x12)
sw      x0,  136(x12)
lw      x18, 140(x12)
sw      x0,  144(x12)
lw      x20, 148(x12)
sw      x0,  152(x12)
lw      x22, 156(x12)
addi    x31, x0,  0x7AE

# TARGET: alternating mispredict in a loop (taken / not taken every other
# iteration) with a store on both sides
mm_part3:
addi    x17, x0,  24
addi    x18, x9,  1024
addi    x18, x18, 1024
addi    x18, x18, 1024       # 0x2C00
alt_loop:
andi    x19, x17, 1
bne     x19, x0,  alt_odd     # taken on odd iterations only
sw      x17, 0(x18)
jal     x0,  alt_next
alt_odd:
sub     x20, x0,  x17
sw      x20, 0(x18)
alt_next:
addi    x18, x18, 4
addi    x17, x17, -1
bne     x17, x0,  alt_loop
lw      x21, -4(x18)
lw      x22, -8(x18)
lw      x23, -96(x18)
add     x31, x31, x21
add     x31, x31, x22
xor     x31, x31, x23

# Non-speculative stores still pending when the branch is fetched
sw      x4,  0(x3)
sw      x6,  4(x3)
sb      x5,  8(x3)
sb      x6,  9(x3)

# -------------------- ONE BRANCH (misprediction) --------------------
# 100 (from the list) < 50 is false, but static prediction = taken:
# wrong_path is fetched speculatively and must be flushed.
lw      x26, 0(x1)
lw      x26, 0(x26)
lw      x26, 0(x26)
lw      x26, 0(x26)           # 100
blt     x26, x6,  wrong_path

# ========== FALL-THROUGH PATH (executed) ============================
lw      x7,  0(x3)            # 0x89ABCDEF
lw      x8,  4(x3)            # 50
lw      x9,  8(x3)
lb      x10, 9(x3)            # 50
add     x31, x31, x7
add     x31, x31, x8
xor     x31, x31, x9
add     x31, x31, x10
lw      x11, 12(x2)
lw      x12, 16(x2)
or      x13, x11, x12
sw      x13, 12(x3)
lw      x14, 0(x1)            # 0x110
lw      x15, 4(x14)           # 0 (never written)
sw      x14, 16(x3)
sw      x15, 20(x3)
lb      x16, 0(x3)            # 0xFFFFFFEF
sb      x16, 24(x3)
lw      x17, 24(x3)
add     x31, x31, x17
sw      x31, 28(x3)

# Second mispredict: condition from the checksum just stored
lw      x18, 28(x3)
blt     x18, x18, wrong_path2 # never taken
sw      x18, 32(x3)

# Subroutine with mispredicts inside
addi    x10, x0, 0x180
jal     x30, sum4
sw      x10, 36(x3)
jal     x0,  readback

# x10 = sum of the non-negative words among 4 at x10
sum4:
addi    x11, x0, 4
addi    x12, x0, 0
sum4_loop:
lw      x13, 0(x10)
blt     x13, x0,  sum4_neg    # taken only for negative words
add     x12, x12, x13
sum4_neg:
addi    x10, x10, 4
addi    x11, x11, -1
bne     x11, x0,  sum4_loop
addi    x10, x12, 0
jalr    x0,  x30, 0

# ========== SECOND WRONG PATH (flushed) =============================
wrong_path2:
sw      x0,  32(x3)
sw      x0,  0x180(x0)
addi    x31, x0, 0x7AE
jalr    x0,  x30, 0

# ========== WRONG PATH (flushed - must NOT execute) =================
wrong_path:
sw      x0,  0(x3)
sw      x0,  4(x3)
sw      x0,  8(x3)
sw      x0,  0(x2)
sw      x0,  4(x2)
sw      x0,  8(x2)
sw      x0,  12(x2)
sw      x0,  16(x2)
sb      x5,  0(x1)
sb      x5,  1(x1)
lw      x7,  0(x3)
lw      x8,  4(x3)
lw      x9,  0(x2)
add     x31, x31, x7
addi    x31, x0, 0x7AD
sw      x31, 12(x3)
sw      x31, 16(x3)
sw      x31, 20(x3)
sw      x31, 24(x3)
sw      x31, 28(x3)
lw      x10, 12(x3)
lw      x11, 16(x3)
sw      x10, 0x110(x0)
sw      x11, 0x120(x0)
xor     x12, x10, x11
sll     x13, x12, x5

# ========== READBACK OF THE FINAL MEMORY STATE ======================
readback:
lw      x1,  0x100(x0)
lw      x2,  0x110(x0)
lw      x3,  0x120(x0)
lw      x4,  0x130(x0)
lw      x5,  0x180(x0)
lw      x6,  0x184(x0)
lw      x7,  0x188(x0)
lw      x8,  0x18C(x0)
lw      x9,  0x190(x0)
lw      x10, 0x194(x0)
lw      x11, 0x198(x0)
lw      x12, 0x19C(x0)
lw      x13, 0x1A0(x0)
lw      x14, 0x200(x0)
lw      x15, 0x204(x0)
lw      x16, 0x208(x0)
lw      x17, 0x20C(x0)
lw      x18, 0x210(x0)
lw      x19, 0x214(x0)
lw      x20, 0x218(x0)
lw      x21, 0x21C(x0)
lb      x22, 0x203(x0)
lb      x23, 0x218(x0)
lb      x24, 0x101(x0)
lw      x25, 0x220(x0)
lw      x26, 0x224(x0)

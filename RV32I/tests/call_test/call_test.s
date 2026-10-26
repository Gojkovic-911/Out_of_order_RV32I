nop
# ===================================================================
# MIXED TEST - JALR
# The first jalr target is computed with auipc, stored to memory and
# loaded back, so the jump depends on a load. Then subroutine calls:
# plain and nested (return address saved on a stack in memory).
# JALR has no prediction: every fall-through is flushed at commit.
# x30 = return address, x29 = stack pointer
# Golden interpreter supports only lw/sw/lb/sb for memory.
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x1,  x0, 0x100        # array
addi    x2,  x0, 0x380        # jump table
lui     x3,  0x13579
addi    x3,  x3, -1347        # 0x13578ABD
addi    x4,  x0, -1
addi    x31, x0, 0            # checksum

# -------------------- JUMP TARGET TO MEMORY -------------------------
target_pc:
auipc   x5,  0
addi    x5,  x5, 172           # address of jalr_target (0x000000c8)
sw      x5,  0(x2)

# -------------------- ALU / MEMORY MIX ------------------------------
add     x6,  x3,  x3
sw      x6,  0(x1)
srai    x7,  x6,  3
sw      x7,  4(x1)
xor     x8,  x6,  x7
sw      x8,  8(x1)
sltu    x9,  x7,  x8
sb      x9,  12(x1)
sb      x4,  13(x1)
sb      x3,  14(x1)
sb      x6,  15(x1)
lw      x10, 12(x1)
lb      x11, 13(x1)
lb      x12, 14(x1)
add     x31, x31, x10
add     x31, x31, x11
xor     x31, x31, x12
lw      x13, 0(x1)
lw      x14, 4(x1)
sub     x15, x13, x14
sw      x15, 16(x1)
lw      x16, 16(x1)
or      x17, x16, x9
andi    x18, x17, 0x7FF
ori     x19, x18, 0x400
sw      x19, 20(x1)
add     x31, x31, x19

# Stores still pending when the jalr is fetched
sw      x31, 24(x1)
sw      x3,  28(x1)

# -------------------- ONE JALR (target loaded from memory) ----------
lw      x20, 0(x2)
jalr    x21, x20, 0           # x21 = return address

# ========== FALL-THROUGH (flushed - must NOT execute) ===============
sw      x0,  0(x1)
sw      x0,  4(x1)
sw      x0,  24(x1)
sb      x4,  0(x2)
lw      x22, 0(x1)
addi    x31, x0, 0x7AD
sw      x31, 28(x1)
add     x23, x22, x31
sw      x23, 32(x1)

# ========== JALR TARGET =============================================
jalr_target:
sw      x21, 32(x1)           # return address to memory
lw      x22, 24(x1)           # checksum stored before the jalr
lw      x23, 28(x1)
add     x24, x22, x23
sw      x24, 36(x1)
lw      x25, 0(x2)            # jump table must still hold the target
sub     x26, x25, x21
sw      x26, 40(x1)
jal     x27, skip
addi    x31, x0, 0x7AD        # skipped
skip:
sw      x27, 44(x1)
slti    x28, x4,  0
sw      x28, 48(x1)


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: link / result consumed right after the return (the rename
# restore at the jalr flush must include the result), 20 calls in a loop
addi    x7,  x0,  20
addi    x10, x0,  0
link_loop:
jal     x30, inc3
add     x31, x31, x10
addi    x7,  x7,  -1
bne     x7,  x0,  link_loop
jal     x0,  link_done
inc3:
addi    x10, x10, 3
jalr    x0,  x30, 0
link_done:

# TARGET: store -> load -> jalr (indirect call through a table entry that
# was just rewritten)
jal     x8,  over_fa
fa:
addi    x10, x10, 100
jalr    x0,  x30, 0
over_fa:
jal     x9,  over_fb
fb:
addi    x10, x10, -50
jalr    x0,  x30, 0
over_fb:
sw      x8,  0x3D0(x0)
lw      x11, 0x3D0(x0)
jalr    x30, x11, 0           # fa
sw      x9,  0x3D0(x0)
lw      x11, 0x3D0(x0)
jalr    x30, x11, 0           # fb
add     x31, x31, x10

# TARGET: function whose first instruction is a branch, called with a
# taken and a not taken outcome
addi    x10, x0,  -9
jal     x30, absval
add     x31, x31, x10
addi    x10, x0,  9
jal     x30, absval
add     x31, x31, x10
jal     x0,  abs_done
absval:
bge     x10, x0,  abs_pos
sub     x10, x0,  x10
abs_pos:
jalr    x0,  x30, 0
abs_done:

# TARGET: three-deep nested calls, return addresses on the stack
addi    x29, x0,  0x3F0
addi    x10, x0,  1
jal     x30, n1
add     x31, x31, x10
jal     x0,  nest_done
n1:
addi    x29, x29, -4
sw      x30, 0(x29)
slli    x10, x10, 1
jal     x30, n2
lw      x30, 0(x29)
addi    x29, x29, 4
jalr    x0,  x30, 0
n2:
addi    x29, x29, -4
sw      x30, 0(x29)
addi    x10, x10, 5
jal     x30, n3
lw      x30, 0(x29)
addi    x29, x29, 4
jalr    x0,  x30, 0
n3:
xori    x10, x10, 0x7F
jalr    x0,  x30, 0
nest_done:

# -------------------- CALLS -----------------------------------------
addi    x29, x0, 0x3F0
addi    x10, x0, 0x100
jal     x30, sum_bytes
sw      x10, 52(x1)
jal     x30, outer
sw      x10, 56(x1)
jal     x0,  readback

# x10 = sum of |byte| for the 4 signed bytes at x10 + 12
sum_bytes:
addi    x11, x10, 12
addi    x12, x10, 16
addi    x10, x0,  0
sum_bytes_loop:
lb      x13, 0(x11)
blt     x13, x0,  sum_bytes_neg
add     x10, x10, x13
jal     x0,  sum_bytes_next
sum_bytes_neg:
sub     x10, x10, x13
sum_bytes_next:
addi    x11, x11, 1
bne     x11, x12, sum_bytes_loop
jalr    x0,  x30, 0

# nested: x10 = sum_bytes(0x100) xor 0x3C, return address on the stack
outer:
addi    x29, x29, -4
sw      x30, 0(x29)
addi    x10, x0, 0x100
jal     x30, sum_bytes
xori    x10, x10, 0x3C
lw      x30, 0(x29)
addi    x29, x29, 4
jalr    x0,  x30, 0

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
lw      x14, 0x380(x0)
lb      x15, 0x10D(x0)
lb      x16, 0x10F(x0)
lw      x17, 0x134(x0)
lw      x18, 0x138(x0)
lw      x19, 0x3EC(x0)        # return address left on the stack

nop
# ===================================================================
# MANY BRANCHES IN FLIGHT - STRAIGHT LINE
# Forward branches of every type back to back (plus jalr), with
# conditions coming from loads and ALU results. Static prediction =
# taken, about half of them are not taken. Each skipped block would
# corrupt x31 / memory if it ever committed. At the end a subroutine
# with branches inside is called through a pointer from memory.
# x30 = return address
# Golden interpreter supports only lw/sw/lb/sb for memory.
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x1,  x0, 0x100        # operand table
addi    x2,  x0, 5
sw      x2,  0(x1)            # 5
addi    x2,  x0, -5
sw      x2,  4(x1)            # -5
lui     x2,  0x80000
addi    x3,  x2, -1
sw      x3,  8(x1)            # 0x7FFFFFFF
sw      x2,  12(x1)           # 0x80000000
addi    x2,  x0, 5
sw      x2,  16(x1)           # 5
addi    x30, x0, 0            # executed blocks
addi    x31, x0, 0            # checksum

lw      x2,  0(x1)            # 5
lw      x3,  4(x1)            # -5
lw      x4,  8(x1)            # 0x7FFFFFFF
lw      x5,  12(x1)           # 0x80000000
lw      x6,  16(x1)           # 5

# -------------------- BRANCH CHAIN ----------------------------------
beq     x2,  x6,  b1          # 5 == 5: taken
addi    x31, x31, 0x100
sw      x31, 0x200(x0)
b1:
addi    x30, x30, 1
bne     x2,  x6,  b2          # not taken
addi    x31, x31, 3
sw      x31, 0x204(x0)
b2:
blt     x3,  x2,  b3          # -5 < 5: taken
addi    x31, x31, 0x200
sw      x31, 0x208(x0)
b3:
bltu    x3,  x2,  b4          # 0xFFFFFFFB <u 5: not taken
addi    x31, x31, 5
sw      x31, 0x20C(x0)
b4:
bge     x4,  x5,  b5          # signed 0x7FFFFFFF >= 0x80000000: taken
addi    x31, x31, 0x300
sw      x0,  0x100(x0)
b5:
bgeu    x4,  x5,  b6          # unsigned: not taken
addi    x31, x31, 7
sw      x31, 0x210(x0)
b6:
beq     x0,  x0,  b7          # taken, target is the next instruction
b7:
bne     x0,  x0,  b8          # not taken, target is the next instruction
b8:
addi    x30, x30, 1

# Condition from a load of a value stored right before
sw      x2,  0x214(x0)
lw      x7,  0x214(x0)
beq     x7,  x2,  b9          # taken
addi    x31, x31, 0x400
sw      x31, 0x218(x0)
b9:

# -------------------- JALR BETWEEN THE BRANCHES ---------------------
jalr_base:
auipc   x8,  0
addi    x8,  x8,  20          # address of after_jalr (0xcc)
jalr    x9,  x8,  0
addi    x31, x31, 0x500       # flushed
sw      x31, 0x21C(x0)
after_jalr:
sub     x10, x8,  x9          # target - return address
sw      x10, 0x220(x0)

# -------------------- MORE BRANCHES ---------------------------------
blt     x2,  x3,  b10         # 5 < -5: not taken
addi    x31, x31, 11
b10:
bge     x3,  x2,  b11         # -5 >= 5: not taken
addi    x31, x31, 13
sw      x31, 0x224(x0)
b11:
slt     x11, x3,  x2          # 1
sltu    x12, x3,  x2          # 0
bne     x11, x12, b12         # taken
addi    x31, x31, 0x600
b12:
add     x13, x11, x12
beq     x13, x0,  b13         # not taken
addi    x30, x30, 1
sw      x30, 0x228(x0)
b13:
lw      x14, 0x100(x0)        # 5, b4 must not have cleared it
bltu    x14, x2,  b14         # 5 <u 5: not taken
addi    x31, x31, 17
b14:
bgeu    x14, x2,  b15         # 5 >=u 5: taken
addi    x31, x31, 0x700
b15:
sw      x31, 0x22C(x0)


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: branches comparing a register with itself
addi    x7,  x0,  5
beq     x7,  x7,  s1          # taken
addi    x31, x0,  0x7AD
s1:
bne     x7,  x7,  s2          # not taken
addi    x31, x31, 1
s2:
blt     x7,  x7,  s3          # not taken
addi    x31, x31, 2
s3:
bge     x7,  x7,  s4          # taken
addi    x31, x0,  0x7AE
s4:
bltu    x7,  x7,  s5          # not taken
addi    x31, x31, 4
s5:
bgeu    x7,  x7,  s6          # taken
addi    x31, x0,  0x7AF
s6:

# TARGET: x0 operands in every branch type
blt     x0,  x0,  z1          # not taken
addi    x31, x31, 8
z1:
bgeu    x0,  x0,  z2          # taken
addi    x31, x0,  0x7B0
z2:
bltu    x0,  x7,  z3          # taken
addi    x31, x0,  0x7B1
z3:
bge     x0,  x7,  z4          # not taken
addi    x31, x31, 16
z4:

# TARGET: signed / unsigned comparator corners (0x80000000, 0x7FFFFFFF,
# -1, 0, 1) through all six branch types - x12 counts the not taken ones
lui     x7,  0x80000          # 0x80000000
addi    x8,  x7,  -1          # 0x7FFFFFFF
addi    x9,  x0,  -1
addi    x10, x0,  0
addi    x11, x0,  1
addi    x12, x0,  0
beq     x7,  x8,  cc0
addi    x12, x12, 1
cc0:
bne     x7,  x8,  cc1
addi    x12, x12, 2
cc1:
blt     x7,  x8,  cc2
addi    x12, x12, 3
cc2:
bge     x7,  x8,  cc3
addi    x12, x12, 4
cc3:
bltu    x7,  x8,  cc4
addi    x12, x12, 5
cc4:
bgeu    x7,  x8,  cc5
addi    x12, x12, 6
cc5:
beq     x8,  x7,  cc6
addi    x12, x12, 7
cc6:
bne     x8,  x7,  cc7
addi    x12, x12, 8
cc7:
blt     x8,  x7,  cc8
addi    x12, x12, 9
cc8:
bge     x8,  x7,  cc9
addi    x12, x12, 10
cc9:
bltu    x8,  x7,  cc10
addi    x12, x12, 11
cc10:
bgeu    x8,  x7,  cc11
addi    x12, x12, 12
cc11:
beq     x9,  x10, cc12
addi    x12, x12, 13
cc12:
bne     x9,  x10, cc13
addi    x12, x12, 14
cc13:
blt     x9,  x10, cc14
addi    x12, x12, 15
cc14:
bge     x9,  x10, cc15
addi    x12, x12, 16
cc15:
bltu    x9,  x10, cc16
addi    x12, x12, 17
cc16:
bgeu    x9,  x10, cc17
addi    x12, x12, 18
cc17:
beq     x10, x9,  cc18
addi    x12, x12, 19
cc18:
bne     x10, x9,  cc19
addi    x12, x12, 20
cc19:
blt     x10, x9,  cc20
addi    x12, x12, 21
cc20:
bge     x10, x9,  cc21
addi    x12, x12, 22
cc21:
bltu    x10, x9,  cc22
addi    x12, x12, 23
cc22:
bgeu    x10, x9,  cc23
addi    x12, x12, 24
cc23:
beq     x9,  x11, cc24
addi    x12, x12, 25
cc24:
bne     x9,  x11, cc25
addi    x12, x12, 26
cc25:
blt     x9,  x11, cc26
addi    x12, x12, 27
cc26:
bge     x9,  x11, cc27
addi    x12, x12, 28
cc27:
bltu    x9,  x11, cc28
addi    x12, x12, 29
cc28:
bgeu    x9,  x11, cc29
addi    x12, x12, 30
cc29:
beq     x7,  x10, cc30
addi    x12, x12, 31
cc30:
bne     x7,  x10, cc31
addi    x12, x12, 32
cc31:
blt     x7,  x10, cc32
addi    x12, x12, 33
cc32:
bge     x7,  x10, cc33
addi    x12, x12, 34
cc33:
bltu    x7,  x10, cc34
addi    x12, x12, 35
cc34:
bgeu    x7,  x10, cc35
addi    x12, x12, 36
cc35:
beq     x10, x7,  cc36
addi    x12, x12, 37
cc36:
bne     x10, x7,  cc37
addi    x12, x12, 38
cc37:
blt     x10, x7,  cc38
addi    x12, x12, 39
cc38:
bge     x10, x7,  cc39
addi    x12, x12, 40
cc39:
bltu    x10, x7,  cc40
addi    x12, x12, 41
cc40:
bgeu    x10, x7,  cc41
addi    x12, x12, 42
cc41:
beq     x11, x10, cc42
addi    x12, x12, 43
cc42:
bne     x11, x10, cc43
addi    x12, x12, 44
cc43:
blt     x11, x10, cc44
addi    x12, x12, 45
cc44:
bge     x11, x10, cc45
addi    x12, x12, 46
cc45:
bltu    x11, x10, cc46
addi    x12, x12, 47
cc46:
bgeu    x11, x10, cc47
addi    x12, x12, 48
cc47:
add     x31, x31, x12

# TARGET: 20 branches in flight behind a slow head (conditions from a
# load pointer chase), alternating outcomes
lui     x13, 0x2
addi    x17, x13, 64
sw      x17, 0(x13)
addi    x18, x13, 128
sw      x18, 0(x17)
addi    x19, x13, 192
sw      x19, 0(x18)
sw      x13, 0(x19)
lw      x14, 0(x13)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x15, 0(x13)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
lw      x15, 0(x15)
addi    x16, x0,  0
beq     x14, x15, fl0
addi    x16, x16, 1
fl0:
bne     x14, x15, fl1
addi    x16, x16, 2
fl1:
beq     x14, x15, fl2
addi    x16, x16, 3
fl2:
bne     x14, x15, fl3
addi    x16, x16, 4
fl3:
beq     x14, x15, fl4
addi    x16, x16, 5
fl4:
bne     x14, x15, fl5
addi    x16, x16, 6
fl5:
beq     x14, x15, fl6
addi    x16, x16, 7
fl6:
bne     x14, x15, fl7
addi    x16, x16, 8
fl7:
beq     x14, x15, fl8
addi    x16, x16, 9
fl8:
bne     x14, x15, fl9
addi    x16, x16, 10
fl9:
beq     x14, x15, fl10
addi    x16, x16, 11
fl10:
bne     x14, x15, fl11
addi    x16, x16, 12
fl11:
beq     x14, x15, fl12
addi    x16, x16, 13
fl12:
bne     x14, x15, fl13
addi    x16, x16, 14
fl13:
beq     x14, x15, fl14
addi    x16, x16, 15
fl14:
bne     x14, x15, fl15
addi    x16, x16, 16
fl15:
beq     x14, x15, fl16
addi    x16, x16, 17
fl16:
bne     x14, x15, fl17
addi    x16, x16, 18
fl17:
beq     x14, x15, fl18
addi    x16, x16, 19
fl18:
bne     x14, x15, fl19
addi    x16, x16, 20
fl19:
add     x31, x31, x16

# TARGET: branch right after a jalr return
jal     x30, ret_now
beq     x10, x0,  after_ret   # taken (x10 = 0)
addi    x31, x0,  0x7B2
after_ret:
addi    x31, x31, 32
jal     x0,  ret_done
ret_now:
jalr    x0,  x30, 0
ret_done:

# -------------------- CALLS THROUGH A POINTER -----------------------
jal     x20, over_clamp
# x10 = clamp(x10, 0, 100)
clamp:
blt     x10, x0,  clamp_lo
addi    x11, x0,  100
blt     x11, x10, clamp_hi
jalr    x0,  x30, 0
clamp_lo:
addi    x10, x0,  0
jalr    x0,  x30, 0
clamp_hi:
addi    x10, x0,  100
jalr    x0,  x30, 0
over_clamp:
sw      x20, 0x230(x0)
lw      x21, 0x230(x0)
addi    x10, x0,  -20
jalr    x30, x21, 0           # 0
sw      x10, 0x234(x0)
addi    x10, x0,  50
jalr    x30, x21, 0           # 50
sw      x10, 0x238(x0)
addi    x10, x0,  500
jalr    x30, x21, 0           # 100
sw      x10, 0x23C(x0)

# -------------------- READBACK OF THE FINAL MEMORY STATE ------------
lw      x1,  0x100(x0)
lw      x2,  0x104(x0)
lw      x3,  0x108(x0)
lw      x4,  0x10C(x0)
lw      x5,  0x200(x0)
lw      x6,  0x204(x0)
lw      x7,  0x208(x0)
lw      x8,  0x20C(x0)
lw      x9,  0x210(x0)
lw      x10, 0x214(x0)
lw      x11, 0x218(x0)
lw      x12, 0x21C(x0)
lw      x13, 0x220(x0)
lw      x14, 0x224(x0)
lw      x15, 0x228(x0)
lw      x16, 0x22C(x0)
lw      x17, 0x230(x0)
lw      x18, 0x234(x0)
lw      x19, 0x238(x0)
lw      x20, 0x23C(x0)

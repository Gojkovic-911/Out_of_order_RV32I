nop
# ===================================================================
# RAW STRESS TEST - DEPENDENCY CHAINS WITH BRANCHES AND JALR
# Long read-after-write chains packed into subroutines (jal call,
# jalr return), run in a counted loop (backward branch) with a
# data dependent branch on every round's result.
# x1 = return address
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x5,  x0, 5
addi    x6,  x5, 10
addi    x7,  x6, 15
addi    x8,  x7, 20
addi    x9,  x8, 25
addi    x10, x9, 30
addi    x11, x10, 35
addi    x12, x11, 40
addi    x13, x12, 45
addi    x14, x13, 50
addi    x15, x14, 55
addi    x16, x15, 60
addi    x17, x16, 65
addi    x18, x17, 70
addi    x19, x18, 75
addi    x20, x0, 5            # rounds
addi    x21, x0, 0            # odd rounds
addi    x31, x0, 0            # checksum

# -------------------- ROUND LOOP ------------------------------------
round_loop:
jal     x1,  chain_a
jal     x1,  chain_b
andi    x22, x19, 1
beq     x22, x0,  even_round  # data dependent
addi    x21, x21, 1
xor     x31, x31, x19
jal     x0,  round_next
even_round:
add     x31, x31, x19
round_next:
addi    x20, x20, -1
bne     x20, x0,  round_loop
jal     x0,  done

# -------------------- CHAIN A: x[i] = x[i-1] + x[i] -----------------
chain_a:
add     x5,  x5,  x6
add     x6,  x5,  x6
add     x7,  x6,  x7
add     x8,  x7,  x8
add     x9,  x8,  x9
add     x10, x9,  x10
add     x11, x10, x11
add     x12, x11, x12
add     x13, x12, x13
add     x14, x13, x14
add     x15, x14, x15
add     x16, x15, x16
add     x17, x16, x17
add     x18, x17, x18
add     x19, x18, x19
jalr    x0,  x1,  0

# -------------------- CHAIN B: rotated, mixed ops -------------------
chain_b:
add     x11, x10, x12
sub     x12, x11, x13
add     x13, x12, x14
xor     x14, x13, x15
add     x15, x14, x16
sub     x16, x15, x17
add     x17, x16, x18
xor     x18, x17, x19
add     x19, x18, x5
srli    x5,  x19, 3
add     x6,  x5,  x7
add     x7,  x6,  x8
sub     x8,  x7,  x9
add     x9,  x8,  x10
add     x10, x9,  x11
jalr    x0,  x1,  0

# -------------------- RESULT ----------------------------------------
done:

# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: both sources on the same tag - one CDB broadcast must wake
# both operand slots of the same RS entry
addi    x7,  x0, 3
add     x7,  x7,  x7
add     x7,  x7,  x7
add     x7,  x7,  x7
sub     x7,  x7,  x0
add     x7,  x7,  x7
xor     x8,  x7,  x7          # 0
or      x8,  x8,  x7
add     x8,  x8,  x8
add     x31, x31, x8

# TARGET: WAW churn on one architectural register - rename table and
# free list must hand the registers back in the right order
addi    x9,  x0, 1
addi    x9,  x9, 2
addi    x9,  x0, 100
addi    x9,  x9, -1
addi    x9,  x0, 7
slli    x9,  x9, 3
addi    x9,  x9, 5
add     x9,  x9,  x9
addi    x9,  x9, -9
add     x31, x31, x9

# TARGET: free list exhaustion - a slow head (load pointer chase) keeps
# 45 independent writers from committing, rename stalls and resumes
lui     x10, 0x2              # ring at 0x2000
addi    x11, x10, 64
sw      x11, 0(x10)
addi    x12, x10, 128
sw      x12, 0(x11)
addi    x13, x10, 192
sw      x13, 0(x12)
sw      x10, 0(x13)
lw      x14, 0(x10)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
lw      x14, 0(x14)
add     x31, x31, x14
addi    x15, x0, 1
addi    x16, x0, 8
addi    x17, x0, 15
addi    x18, x0, 22
addi    x19, x0, 29
addi    x20, x0, 36
addi    x21, x0, 43
addi    x22, x0, 50
addi    x23, x0, 57
addi    x24, x0, 64
addi    x25, x0, 71
addi    x26, x0, 78
addi    x27, x0, 85
addi    x15, x0, 92
addi    x16, x0, 99
addi    x17, x0, 106
addi    x18, x0, 113
addi    x19, x0, 120
addi    x20, x0, 127
addi    x21, x0, 134
addi    x22, x0, 141
addi    x23, x0, 148
addi    x24, x0, 155
addi    x25, x0, 162
addi    x26, x0, 169
addi    x27, x0, 176
addi    x15, x0, 183
addi    x16, x0, 190
addi    x17, x0, 197
addi    x18, x0, 204
addi    x19, x0, 211
addi    x20, x0, 218
addi    x21, x0, 225
addi    x22, x0, 232
addi    x23, x0, 239
addi    x24, x0, 246
addi    x25, x0, 253
addi    x26, x0, 260
addi    x27, x0, 267
addi    x15, x0, 274
addi    x16, x0, 281
addi    x17, x0, 288
addi    x18, x0, 295
addi    x19, x0, 302
addi    x20, x0, 309
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

# TARGET: x0 as destination (never renamed, never written) and source
add     x0,  x14, x14
addi    x0,  x0,  123
lw      x0,  0(x10)
lui     x0,  0x12345
add     x16, x0,  x0
addi    x16, x16, 1           # 1 only if x0 stayed 0
beq     x0,  x0,  x0_ok
addi    x16, x0,  0x7AD
x0_ok:
add     x31, x31, x16

# TARGET: producer -> consumer at every distance 0..3 (RS entry write
# bypass, IQ wakeup, regfile read), repeated in a loop
addi    x17, x0, 12
addi    x18, x0, 1
dist_loop:
addi    x19, x18, 1           # distance 0
add     x20, x19, x19
addi    x21, x20, 3           # distance 1
xor     x22, x18, x17
add     x23, x21, x18
addi    x24, x23, -2          # distance 2
or      x25, x17, x18
and     x26, x17, x18
sub     x27, x24, x25         # distance 3
add     x18, x27, x26
andi    x18, x18, 0x3FF
add     x31, x31, x18
addi    x17, x17, -1
bne     x17, x0,  dist_loop

add     x30, x21, x31
sub     x29, x19, x5

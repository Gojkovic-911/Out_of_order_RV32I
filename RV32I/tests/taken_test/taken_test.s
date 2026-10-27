# ===================================================================
# CORRECTLY PREDICTED BRANCHES
# Static prediction = taken. A chain of forward branches that are all
# taken (each skips a block that would corrupt the result), a counted
# loop whose backward branch is right on every iteration but the
# last, and a subroutine called with jal and returned with jalr.
# No loads/stores. x1 = return address
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x5,  x0, 1
addi    x6,  x0, 100
addi    x7,  x0, 50
addi    x8,  x0, -7
addi    x9,  x0, 9
addi    x10, x0, 10
addi    x11, x0, 11
addi    x12, x0, 12
addi    x31, x0, 0            # checksum

# -------------------- CHAIN OF TAKEN BRANCHES -----------------------
add     x13, x9,  x10         # 19
blt     x7,  x6,  t1          # 50 < 100
addi    x31, x0,  0x7AD
t1:
add     x14, x13, x11         # 30
bge     x6,  x7,  t2          # 100 >= 50
addi    x31, x0,  0x7AD
t2:
sub     x15, x14, x12         # 18
bne     x15, x13, t3          # 18 != 19
addi    x31, x0,  0x7AD
t3:
xor     x16, x15, x8
bltu    x5,  x8,  t4          # 1 <u 0xFFFFFFF9
addi    x31, x0,  0x7AD
t4:
or      x17, x16, x9
bgeu    x8,  x6,  t5          # 0xFFFFFFF9 >=u 100
addi    x31, x0,  0x7AD
t5:
and     x18, x17, x14
addi    x19, x18, 0
beq     x19, x18, t6          # equal
addi    x31, x0,  0x7AD
t6:
add     x31, x31, x13
add     x31, x31, x14
add     x31, x31, x15
xor     x31, x31, x16
add     x31, x31, x17
add     x31, x31, x18

# -------------------- COUNTED LOOP ----------------------------------
addi    x20, x0, 0            # i
addi    x21, x0, 12           # N
addi    x22, x0, 0            # sum
addi    x23, x0, 1            # fib a
addi    x24, x0, 1            # fib b
count_loop:
add     x22, x22, x20
add     x25, x23, x24
addi    x23, x24, 0
addi    x24, x25, 0
slli    x26, x20, 2
xor     x31, x31, x26
addi    x20, x20, 1
blt     x20, x21, count_loop

# -------------------- SUBROUTINE CALLS ------------------------------
addi    x10, x22, 0
jal     x1,  mix3
addi    x27, x10, 0
addi    x10, x24, 0
jal     x1,  mix3
addi    x28, x10, 0
jal     x0,  done

# x10 = ((x10 << 3) xor x10) + 0x55
mix3:
slli    x11, x10, 3
xor     x10, x11, x10
addi    x10, x10, 0x55
jalr    x0,  x1,  0

# -------------------- RESULT ----------------------------------------
done:

# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: back-to-back taken branches with nothing in between (every
# target is the next branch)
beq     x0,  x0,  c0
c0:
beq     x0,  x0,  c1
c1:
beq     x0,  x0,  c2
c2:
beq     x0,  x0,  c3
c3:
beq     x0,  x0,  c4
c4:
beq     x0,  x0,  c5
c5:
beq     x0,  x0,  c6
c6:
beq     x0,  x0,  c7
c7:
addi    x31, x31, 1

# TARGET: jal -> jal -> jal (decode redirects on consecutive cycles) and
# the PC auipc sees at the end of the chain
jal     x0,  jc1
jc3:
auipc   x7,  0
jal     x0,  jc_end
jc2:
jal     x0,  jc3
jc1:
jal     x0,  jc2
jc_end:
add     x31, x31, x7

# TARGET: a branch sitting at a jump target, a jal at a branch target
jal     x0,  bt1
addi    x31, x0,  0x7AD
bt1:
bge     x7,  x0,  bt2
addi    x31, x0,  0x7AD
bt2:
jal     x0,  bt3
addi    x31, x0,  0x7AD
bt3:
auipc   x8,  0
add     x31, x31, x8

# TARGET: branch to the next instruction (taken, target = pc + 4)
blt     x0,  x7,  next_instr
next_instr:
addi    x31, x31, 3

# TARGET: tight 2-instruction loop, 50 iterations
addi    x9,  x0, 50
tight_loop:
addi    x9,  x9, -1
bne     x9,  x0, tight_loop
add     x31, x31, x9

# TARGET: far offsets - branch forward and backward over more than 2 KB
# (immediate bit 11 and up), jal over the same distance
addi    x10, x0, 1
addi    x28, x0, 0
jal     x0,  skip_back_target
back_target:
addi    x11, x0, 0x55
jal     x0,  after_back
skip_back_target:
bne     x10, x0,  far_fwd     # > 2 KB forward
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
addi    x28, x28, 1
far_fwd:
auipc   x12, 0
add     x31, x31, x12
add     x31, x31, x28         # padding never executed -> 0
blt     x0,  x10, back_target # > 2 KB backward
after_back:
add     x31, x31, x11
jal     x13, far_jal_back     # jal to the next instruction (link = target)
far_jal_back:
add     x31, x31, x13

add     x29, x27, x28
add     x30, x31, x29

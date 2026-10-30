# ===================================================================
# LOOPS - MANY BRANCHES IN FLIGHT
# Static prediction = taken: a backward loop branch is right on every
# iteration but the last one, an if/else inside the loop mispredicts
# on part of the iterations while younger loop branches are already
# in flight. Loops write and read memory every iteration.
# A subroutine is called from inside a loop (jalr return every
# iteration). x30 = return address
# Golden interpreter supports only lw/sw/lb/sb for memory.
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x1,  x0, 0x100        # array A
addi    x2,  x0, 0x200        # array B
addi    x3,  x0, 16           # N
addi    x31, x0, 0            # checksum

# -------------------- INIT LOOP: A[i] = 7*i - 40 --------------------
addi    x4,  x0, 0            # i
addi    x5,  x1, 0            # &A[i]
init_loop:
slli    x6,  x4,  3
sub     x7,  x6,  x4
addi    x8,  x7,  -40
sw      x8,  0(x5)
addi    x5,  x5,  4
addi    x4,  x4,  1
blt     x4,  x3,  init_loop

# -------------------- ABS LOOP: B[i] = |A[i]| -----------------------
# bge is taken (predicted right) only for A[i] >= 0, i.e. i >= 6
addi    x4,  x0, 0
addi    x5,  x1, 0
addi    x6,  x2, 0
abs_loop:
lw      x7,  0(x5)
bge     x7,  x0,  positive
sub     x7,  x0,  x7          # negative: negate
sw      x7,  0(x6)
jal     x0,  abs_next
positive:
add     x31, x31, x7          # sum of the positive ones
sw      x7,  0(x6)
abs_next:
addi    x5,  x5,  4
addi    x6,  x6,  4
addi    x4,  x4,  1
bne     x4,  x3,  abs_loop

# -------------------- COPY LOOP: C[k] = B[N-1-k] (count down) -------
addi    x4,  x3,  -1          # i = N-1
addi    x8,  x0,  0x300       # &C[0]
copy_loop:
slli    x9,  x4,  2
add     x10, x2,  x9
lw      x11, 0(x10)
sw      x11, 0(x8)
addi    x8,  x8,  4
addi    x4,  x4,  -1
bge     x4,  x0,  copy_loop

# -------------------- NESTED LOOP: row sums of C as 4x4 -------------
addi    x13, x0,  0           # row
addi    x14, x0,  0x300       # &C[row][col]
addi    x17, x0,  4
outer_loop:
addi    x12, x0,  0           # row sum
addi    x15, x0,  0           # col
inner_loop:
lw      x16, 0(x14)
add     x12, x12, x16
addi    x14, x14, 4
addi    x15, x15, 1
blt     x15, x17, inner_loop
slli    x18, x13, 2
sw      x12, 0x380(x18)
xor     x31, x31, x12
addi    x13, x13, 1
blt     x13, x17, outer_loop


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: 100 iterations with register churn and memory traffic - ROB,
# free list, IQ and LSQ indices all wrap around many times
lui     x7,  0x1              # 0x1000
addi    x8,  x0,  100
addi    x9,  x0,  0
churn_loop:
addi    x10, x8,  3
xor     x11, x10, x9
slli    x12, x11, 2
andi    x12, x12, 0xFC
add     x13, x7,  x12
sw      x10, 0(x13)
lw      x14, 0(x13)
add     x9,  x9,  x14
srli    x15, x9,  1
xor     x9,  x9,  x15
addi    x8,  x8,  -1
bne     x8,  x0,  churn_loop
add     x31, x31, x9

# TARGET: inner loop with trip count 1 - its exit mispredicts every time
# while the outer loop branch is in flight behind it
addi    x16, x0,  15
addi    x17, x0,  0
outer1:
addi    x18, x0,  1
inner1:
add     x17, x17, x16
addi    x18, x18, -1
bne     x18, x0,  inner1
addi    x16, x16, -1
bne     x16, x0,  outer1
add     x31, x31, x17

# TARGET: top-tested while loop - the forward exit branch is predicted
# taken and wrong on every iteration but the last one
addi    x19, x0,  0
addi    x20, x0,  0
while_top:
addi    x21, x0,  20
bge     x19, x21, while_end
add     x20, x20, x19
addi    x19, x19, 1
jal     x0,  while_top
while_end:
add     x31, x31, x20

# TARGET: memset then checksum over 64 words above 2 KB, count-down loops
lui     x22, 0x1
addi    x22, x22, 0x400       # 0x1400
addi    x23, x0,  64
addi    x24, x22, 0
memset_loop:
sw      x23, 0(x24)
addi    x24, x24, 4
addi    x23, x23, -1
bne     x23, x0,  memset_loop
addi    x23, x0,  64
addi    x25, x0,  0
csum_loop:
addi    x24, x24, -4
lw      x26, 0(x24)
add     x25, x25, x26
addi    x23, x23, -1
bne     x23, x0,  csum_loop
add     x31, x31, x25

# -------------------- CALL INSIDE A LOOP: E[i] = tri(C[i]) ---------
addi    x4,  x0, 0
addi    x5,  x0, 0x300
addi    x6,  x0, 32
call_loop:
lw      x10, 0(x5)
jal     x30, tri
sw      x10, 0x3C0(x4)
add     x31, x31, x10
addi    x5,  x5,  4
addi    x4,  x4,  4
blt     x4,  x6,  call_loop
jal     x0,  readback
# x10 = 3 * x10 - (x10 >> 1)
tri:
slli    x11, x10, 1
add     x11, x11, x10
srai    x12, x10, 1
sub     x10, x11, x12
jalr    x0,  x30, 0

# -------------------- READBACK OF THE FINAL MEMORY STATE ------------
readback:
lw      x1,  0x100(x0)
lw      x2,  0x114(x0)
lw      x3,  0x118(x0)
lw      x4,  0x13C(x0)
lw      x5,  0x200(x0)
lw      x6,  0x204(x0)
lw      x7,  0x210(x0)
lw      x8,  0x214(x0)
lw      x9,  0x218(x0)
lw      x10, 0x21C(x0)
lw      x11, 0x220(x0)
lw      x12, 0x23C(x0)
lw      x13, 0x300(x0)
lw      x14, 0x304(x0)
lw      x15, 0x320(x0)
lw      x16, 0x324(x0)
lw      x17, 0x338(x0)
lw      x18, 0x33C(x0)
lw      x19, 0x380(x0)
lw      x20, 0x384(x0)
lw      x21, 0x388(x0)
lw      x22, 0x38C(x0)
lb      x23, 0x100(x0)
lb      x24, 0x13C(x0)
lw      x25, 0x3C0(x0)
lw      x26, 0x3DC(x0)

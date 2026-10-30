# ===================================================================
# MISPREDICTED BRANCHES
# Static prediction = taken. Several forward branches that are NOT
# taken, so each predicted (taken) target is a wrong path that gets
# flushed. The wrong paths are long and contain branches, jal and
# jalr themselves. Branches are close enough to be in flight together.
# No loads/stores. x1 = return address
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x5,  x0, 1
addi    x6,  x0, 100
addi    x7,  x0, 50
addi    x8,  x0, 8
addi    x9,  x0, 9
addi    x10, x0, 10
addi    x31, x0, 0            # checksum

# -------------------- LONG CHAIN, THEN THE FIRST BRANCH -------------
add     x11, x9,  x10
add     x12, x11, x10
add     x13, x12, x11
add     x14, x13, x12
add     x15, x14, x13
add     x16, x15, x14
blt     x6,  x7,  wrong1      # 100 < 50: not taken
add     x31, x31, x16

# Second and third branches right behind the first one
bge     x7,  x6,  wrong2      # 50 >= 100: not taken
sub     x17, x16, x15
beq     x17, x16, wrong3      # not taken
add     x31, x31, x17
jal     x0,  part2

# ========== WRONG PATHS (flushed - must NOT execute) ================
wrong1:
addi    x31, x0,  0x7AD
blt     x7,  x6,  wrong1b     # a branch on the wrong path
addi    x11, x0,  0
wrong1b:
jal     x1,  never            # a call on the wrong path
addi    x12, x0,  0
jal     x0,  part2
wrong2:
addi    x31, x0,  0x7AE
add     x13, x0,  x0
jalr    x0,  x1,  0           # jalr on the wrong path
wrong3:
addi    x31, x0,  0x7AF
never:
addi    x14, x0,  0
jalr    x0,  x1,  0

# -------------------- PART 2: MISPREDICT INSIDE A LOOP --------------
# bne exits on the last iteration, the inner blt is never taken
part2:
addi    x20, x0, 0            # i
addi    x21, x0, 6            # N
loop2:
add     x22, x20, x8
sltu    x23, x22, x9
blt     x22, x5,  skip2       # never taken (x22 >= 8)
add     x31, x31, x22
xor     x31, x31, x23
skip2:
addi    x20, x20, 1
bne     x20, x21, loop2

# -------------------- PART 3: CALL WITH A MISPREDICT INSIDE ---------
addi    x10, x16, 0
jal     x1,  check
addi    x24, x10, 0
addi    x10, x5,  0
jal     x1,  check
addi    x25, x10, 0
jal     x0,  done

# x10 = x10 > 50 ? x10 - 50 : x10 + 1000
check:
bge     x7,  x10, small       # not taken for x10 > 50
addi    x10, x10, -50
jalr    x0,  x1,  0
small:
addi    x10, x10, 1000
jalr    x0,  x1,  0

# -------------------- RESULT ----------------------------------------
done:

# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: mispredicts back to back (each one fetched after the other flush)
addi    x7,  x0, 1
beq     x7,  x0,  m_wrong1    # not taken
beq     x7,  x0,  m_wrong1    # not taken
bne     x7,  x7,  m_wrong1    # not taken
add     x31, x31, x7
jal     x0,  m_part2
m_wrong1:
addi    x31, x0,  0x7AD
# TARGET: a wrong path that spins forever until the flush
m_spin:
jal     x0,  m_spin

# TARGET: wrong path jumping to a garbage (odd, out of program) address
m_part2:
addi    x8,  x0, 2
blt     x8,  x0,  m_wrong2    # not taken
add     x31, x31, x8
jal     x0,  m_part3
m_wrong2:
addi    x9,  x0, 2047
jalr    x0,  x9, 0

# TARGET: rename table restore after heavy renaming on both paths
m_part3:
addi    x10, x0, 1
addi    x11, x0, 2
addi    x10, x10, 10
addi    x11, x11, 20
addi    x10, x10, 100
addi    x11, x11, 200
bge     x0,  x10, m_wrong3    # not taken
add     x31, x31, x10
add     x31, x31, x11
jal     x0,  m_part4
m_wrong3:
addi    x10, x0,  0x7AD
addi    x11, x0,  0x7AE
addi    x10, x10, 1
addi    x11, x11, 1
jal     x0,  m_part4

# TARGET: free list exhausted on the wrong path, branch resolved late
# (condition from a load pointer chase)
m_part4:
lui     x12, 0x2
addi    x13, x12, 64
sw      x13, 0(x12)
addi    x14, x12, 128
sw      x14, 0(x13)
addi    x15, x12, 192
sw      x15, 0(x14)
sw      x12, 0(x15)
lw      x16, 0(x12)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
lw      x16, 0(x16)
bne     x16, x12, m_wrong4    # equal: not taken
add     x31, x31, x16
jal     x0,  m_part5
m_wrong4:
addi    x7,  x0, 11
addi    x8,  x0, 18
addi    x9,  x0, 25
addi    x10, x0, 32
addi    x11, x0, 39
addi    x12, x0, 46
addi    x13, x0, 53
addi    x14, x0, 60
addi    x15, x0, 67
addi    x16, x0, 74
addi    x17, x0, 81
addi    x18, x0, 88
addi    x19, x0, 95
addi    x20, x0, 102
addi    x21, x0, 109
addi    x22, x0, 116
addi    x23, x0, 123
addi    x24, x0, 130
addi    x25, x0, 137
addi    x26, x0, 144
addi    x27, x0, 151
addi    x7,  x0, 158
addi    x8,  x0, 165
addi    x9,  x0, 172
addi    x10, x0, 179
addi    x11, x0, 186
addi    x12, x0, 193
addi    x13, x0, 200
addi    x14, x0, 207
addi    x15, x0, 214
addi    x16, x0, 221
addi    x17, x0, 228
addi    x18, x0, 235
addi    x19, x0, 242
addi    x20, x0, 249
addi    x21, x0, 256
addi    x22, x0, 263
addi    x23, x0, 270
addi    x24, x0, 277
addi    x25, x0, 284
addi    x26, x0, 291
addi    x27, x0, 298
addi    x31, x0,  0x7AF

# TARGET: branch whose condition comes from store -> load
m_part5:
addi    x17, x0, 0x123
sw      x17, 0x40(x12)
lw      x18, 0x40(x12)
bne     x18, x17, m_wrong5    # not taken
add     x31, x31, x18
jal     x0,  m_part6
m_wrong5:
sw      x0,  0x40(x12)
addi    x31, x0,  0x7B0

# TARGET: mispredict inside a function, the jalr return right after it
m_part6:
addi    x19, x0, 5
jal     x30, m_fun
add     x31, x31, x19
jal     x0,  m_end
m_fun:
bltu    x19, x0,  m_fun_wrong # never taken
addi    x19, x19, 7
jalr    x0,  x30, 0
m_fun_wrong:
addi    x19, x0,  0x7B1
jalr    x0,  x30, 0
m_end:

add     x26, x24, x25
add     x30, x31, x26

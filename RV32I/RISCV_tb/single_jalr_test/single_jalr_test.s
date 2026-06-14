nop
# ===================================================================
# BIG STRESS TEST – ONE BRANCH MISPREDICTION + JAL (no JALR)
# No loads/stores. Only ALU + JAL.
# Static prediction = taken; actual branch = not taken.
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x1,  x0, 1
addi    x2,  x0, 100         # for branch condition (false)
addi    x3,  x0, 50          # for branch condition
addi    x4,  x0, 4
addi    x5,  x0, 5
addi    x6,  x0, 6
addi    x7,  x0, 7
addi    x8,  x0, 8
addi    x9,  x0, 9
addi    x10, x0, 10
addi    x11, x0, 11
addi    x12, x0, 12
addi    x13, x0, 13
addi    x14, x0, 14
addi    x15, x0, 15
addi    x16, x0, 16
addi    x17, x0, 17
addi    x18, x0, 18
addi    x19, x0, 19
addi    x20, x0, 20
addi    x21, x0, 21
addi    x22, x0, 22
addi    x23, x0, 23
addi    x24, x0, 24
addi    x25, x0, 25
addi    x26, x0, 26
addi    x27, x0, 27
addi    x28, x0, 28
addi    x29, x0, 29
addi    x30, x0, 30
addi    x31, x0, 31

# -------------------- JAL (forward jump, no return) -----------------
# Jump to a label that will continue execution; no JALR used.
jal     x1, after_jal       # x1 = return address (unused), jump forward

# This block is skipped (never executed)
addi    x2,  x0, 0xAD
addi    x3,  x0, 0xEF
add     x4,  x2,  x3

after_jal:
# -------------------- LONG ALU DEPENDENCY CHAIN --------------------
add     x5,  x5,  x6        # 5+6=11
add     x6,  x6,  x7        # 6+7=13
add     x7,  x7,  x8        # 7+8=15
add     x8,  x8,  x9        # 8+9=17
add     x9,  x9,  x10       # 9+10=19
add     x10, x10, x11       # 10+11=21
add     x11, x11, x12       # 11+12=23
add     x12, x12, x13       # 12+13=25
add     x13, x13, x14       # 13+14=27
add     x14, x14, x15       # 14+15=29
add     x15, x15, x16       # 15+16=31
add     x16, x16, x17       # 16+17=33
add     x17, x17, x18       # 17+18=35
add     x18, x18, x19       # 18+19=37
add     x19, x19, x20       # 19+20=39
add     x20, x20, x21       # 20+21=41
add     x21, x21, x22       # 21+22=43
add     x22, x22, x23       # 22+23=45
add     x23, x23, x24       # 23+24=47
add     x24, x24, x25       # 24+25=49
add     x25, x25, x26       # 25+26=51
add     x26, x26, x27       # 26+27=53
add     x27, x27, x28       # 27+28=55
add     x28, x28, x29       # 28+29=57
add     x29, x29, x30       # 29+30=59
add     x30, x30, x31       # 30+31=61
add     x31, x31, x1        # 61 + (return address low bits) – messy but fine

# -------------------- SHIFT CHAIN -----------------------------------
sll     x1,  x5,  x6        # 11 << 13 = 90112
srl     x2,  x6,  x7        # 13 >> 15 = 0
sll     x3,  x7,  x8        # 15 << 17 = 1966080
srl     x4,  x8,  x9        # 17 >> 19 = 0
sll     x5,  x9,  x10       # 19 << 21 = 39845888
srl     x6,  x10, x11       # 21 >> 23 = 0
sll     x7,  x11, x12       # 23 << 25 = 771751936
srl     x8,  x12, x13       # 25 >> 27 = 0
sll     x9,  x13, x14       # 27 << 29 = 14495514624 (wraps)
srl     x10, x14, x15       # 29 >> 31 = 0
sll     x11, x15, x16       # 31 << 33 = 266287972352 (wraps)
srl     x12, x16, x17       # 33 >> 35 = 0
sll     x13, x17, x18       # 35 << 37
srl     x14, x18, x19       # 37 >> 39 = 0
sll     x15, x19, x20       # 39 << 41
srl     x16, x20, x21       # 41 >> 43 = 0
sll     x17, x21, x22       # 43 << 45
srl     x18, x22, x23       # 45 >> 47 = 0
sll     x19, x23, x24       # 47 << 49
srl     x20, x24, x25       # 49 >> 51 = 0

# -------------------- LOGICAL CHAIN --------------------------------
xor     x21, x1,  x2        # 90112 xor 0 = 90112
and     x22, x3,  x4        # 1966080 & 0 = 0
or      x23, x5,  x6        # 39845888 | 0 = 39845888
xor     x24, x7,  x8        # 771751936 xor 0 = 771751936
and     x25, x9,  x10       # ...
or      x26, x11, x12
xor     x27, x13, x14
and     x28, x15, x16
or      x29, x17, x18
xor     x30, x19, x20
and     x31, x21, x22
or      x1,  x23, x24
xor     x2,  x25, x26
and     x3,  x27, x28
or      x4,  x29, x30
xor     x5,  x31, x1
and     x6,  x2,  x3
or      x7,  x4,  x5
xor     x8,  x6,  x7
and     x9,  x8,  x1
or      x10, x9,  x2

# -------------------- SLT / SLTI / SLTIU CHAIN ----------------------
slt     x11, x10, x3
slt     x14, x13, x4
slt     x17, x16, x5
slt     x20, x19, x6
slt     x23, x22, x7
slt     x26, x25, x8
slt     x29, x28, x9

# -------------------- MIXED DEPENDENCY CHAIN (long) -----------------
add     x1,  x31, x10
sub     x2,  x1,  x11
sll     x3,  x2,  x12
srl     x4,  x3,  x13
xor     x5,  x4,  x14
and     x6,  x5,  x15
or      x7,  x6,  x16
add     x8,  x7,  x17
sub     x9,  x8,  x18
sll     x10, x9,  x19
srl     x11, x10, x20
xor     x12, x11, x21
and     x13, x12, x22
or      x14, x13, x23
add     x15, x14, x24
sub     x16, x15, x25
sll     x17, x16, x26
srl     x18, x17, x27
xor     x19, x18, x28
and     x20, x19, x29
or      x21, x20, x30
add     x22, x21, x31
sub     x23, x22, x1
sll     x24, x23, x2
srl     x25, x24, x3
xor     x26, x25, x4
and     x27, x26, x5
or      x28, x27, x6
add     x29, x28, x7
sub     x30, x29, x8
sll     x31, x30, x9
srl     x1,  x31, x10
xor     x2,  x1,  x11
and     x3,  x2,  x12
or      x4,  x3,  x13
add     x5,  x4,  x14
sub     x6,  x5,  x15
sll     x7,  x6,  x16
srl     x8,  x7,  x17
xor     x9,  x8,  x18
and     x10, x9,  x19
or      x11, x10, x20
add     x12, x11, x21
sub     x13, x12, x22
sll     x14, x13, x23
srl     x15, x14, x24
xor     x16, x15, x25
and     x17, x16, x26
or      x18, x17, x27
add     x19, x18, x28
sub     x20, x19, x29
sll     x21, x20, x30
srl     x22, x21, x31
xor     x23, x22, x1
and     x24, x23, x2
or      x25, x24, x3
add     x26, x25, x4
sub     x27, x26, x5
sll     x28, x27, x6
srl     x29, x28, x7
xor     x30, x29, x8
and     x31, x30, x9
or      x1,  x31, x10

# -------------------- ONE JALR -------------------
jalr     x2, x0, 0x364     

# ========== FALL‑THROUGH PATH (executed) ===========================
# Large block of ALU instructions that will not be executed
addi    x2,  x0, 0xCA
addi    x3,  x0, 0xBA
xor     x4,  x2,  x3
and     x5,  x4,  x2
or      x6,  x5,  x3
add     x7,  x6,  x2
sub     x8,  x7,  x3
sll     x9,  x8,  x4
srl     x10, x9,  x5
slt     x11, x10, x6
add     x13, x12, x7
addi    x14, x13, 0xAB
xor     x15, x14, x2
and     x16, x15, x3
or      x17, x16, x4
add     x18, x17, x5
sub     x19, x18, x6
sll     x20, x19, x7
srl     x21, x20, x8
slt     x22, x21, x9
add     x24, x23, x10
addi    x25, x24, 0xEF
xor     x26, x25, x11
and     x27, x26, x12
or      x28, x27, x13
add     x29, x28, x14
sub     x30, x29, x15
sll     x31, x30, x16
srl     x1,  x31, x17
slt     x2,  x1,  x18
add     x4,  x3,  x19
addi    x5,  x4,  0xE
xor     x6,  x5,  x20
and     x7,  x6,  x21
or      x8,  x7,  x22
add     x9,  x8,  x23
sub     x10, x9,  x24
sll     x11, x10, x25
srl     x12, x11, x26
slt     x13, x12, x27
add     x15, x14, x28

# Jump to end to skip target block
jal     x0, end

# ========== TARGET BLOCK (executed) ===========
target:
addi    x1,  x0, 0xDF
addi    x2,  x0, 0xFE
add     x3,  x1,  x2
xor     x4,  x3,  x1
sll     x5,  x4,  x2
srl     x6,  x5,  x3
addi    x7,  x0, 0xCA
addi    x8,  x0, 0xF0
or      x9,  x7,  x8
and     x10, x9,  x1
sub     x11, x10, x2
add     x13, x12, x1
sll     x14, x13, x2
srl     x15, x14, x3
xor     x16, x15, x4
and     x17, x16, x5
or      x18, x17, x6
slt     x19, x18, x7
add     x21, x20, x8
addi    x22, x0, 0x12
addi    x23, x0, 0x56
add     x24, x22, x23
sub     x25, x24, x22
sll     x26, x25, x1
srl     x27, x26, x2
xor     x28, x27, x22
and     x29, x28, x23
or      x30, x29, x24
slt     x31, x30, x25
add     x2,  x1,  x30
# ... many more flushed instructions (add more if desired)

# ========== END ====================================================
end:
nop
nop
nop

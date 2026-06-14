nop 
start:
addi x1,  x0, 1
addi x2,  x0, 2
addi x3,  x0, 3
addi x4,  x0, 4
addi x5,  x0, 5
addi x6,  x0, 6
addi x7,  x0, 7
addi x8,  x0, 8
addi x9,  x0, 9
addi x10, x0, 10
addi x11, x0, 11
addi x12, x0, 12
addi x13, x0, 13
addi x14, x0, 14
addi x15, x0, 15
addi x16, x0, 16

jal x0, phase1

phase1:
add x17, x1, x2
add x18, x17, x3
add x19, x18, x4
add x20, x19, x5
add x21, x20, x6
add x22, x21, x7
add x23, x22, x8
add x24, x23, x9
add x25, x24, x10
add x26, x25, x11
add x27, x26, x12
add x28, x27, x13
add x29, x28, x14
add x30, x29, x15
add x31, x30, x16

xor x17, x31, x30
xor x18, x17, x29
xor x19, x18, x28
xor x20, x19, x27
xor x21, x20, x26
xor x22, x21, x25
xor x23, x22, x24
xor x24, x23, x23
xor x25, x24, x22
xor x26, x25, x21
xor x27, x26, x20
xor x28, x27, x19
xor x29, x28, x18
xor x30, x29, x17
xor x31, x30, x16

or x17, x31, x1
or x18, x17, x2
or x19, x18, x3
or x20, x19, x4
or x21, x20, x5
or x22, x21, x6
or x23, x22, x7
or x24, x23, x8
or x25, x24, x9
or x26, x25, x10
or x27, x26, x11
or x28, x27, x12
or x29, x28, x13
or x30, x29, x14
or x31, x30, x15

jal x0, phase2

phase2:
addi x1, x1, 1
addi x2, x1, 2
addi x3, x2, 3
addi x4, x3, 4
addi x5, x4, 5
addi x6, x5, 6
addi x7, x6, 7
addi x8, x7, 8
addi x9, x8, 9
addi x10, x9, 10
addi x11, x10, 11
addi x12, x11, 12
addi x13, x12, 13
addi x14, x13, 14
addi x15, x14, 15
addi x16, x15, 16
addi x17, x16, 17
addi x18, x17, 18
addi x19, x18, 19
addi x20, x19, 20
addi x21, x20, 21
addi x22, x21, 22
addi x23, x22, 23
addi x24, x23, 24
addi x25, x24, 25
addi x26, x25, 26
addi x27, x26, 27
addi x28, x27, 28
addi x29, x28, 29
addi x30, x29, 30
addi x31, x30, 31

jal x0, phase3

phase3:
and x1,  x31, x30
and x2,  x1,  x29
and x3,  x2,  x28
and x4,  x3,  x27
and x5,  x4,  x26
and x6,  x5,  x25
and x7,  x6,  x24
and x8,  x7,  x23
and x9,  x8,  x22
and x10, x9,  x21
and x11, x10, x20
and x12, x11, x19
and x13, x12, x18
and x14, x13, x17
and x15, x14, x16

add x16, x15, x1
add x17, x16, x2
add x18, x17, x3
add x19, x18, x4
add x20, x19, x5
add x21, x20, x6
add x22, x21, x7
add x23, x22, x8
add x24, x23, x9
add x25, x24, x10
add x26, x25, x11
add x27, x26, x12
add x28, x27, x13
add x29, x28, x14
add x30, x29, x15
add x31, x30, x16

jal x0, phase4

phase4:
xor x1,  x1,  x2
add x2,  x1,  x3
or  x3,  x2,  x4
and x4,  x3,  x5
xor x5,  x4,  x6
add x6,  x5,  x7
or  x7,  x6,  x8
and x8,  x7,  x9
xor x9,  x8,  x10
add x10, x9,  x11
or  x11, x10, x12
and x12, x11, x13
xor x13, x12, x14
add x14, x13, x15
or  x15, x14, x16
and x16, x15, x17
xor x17, x16, x18
add x18, x17, x19
or  x19, x18, x20
and x20, x19, x21
xor x21, x20, x22
add x22, x21, x23
or  x23, x22, x24
and x24, x23, x25
xor x25, x24, x26
add x26, x25, x27
or  x27, x26, x28
and x28, x27, x29
xor x29, x28, x30
add x30, x29, x31
or  x31, x30, x1

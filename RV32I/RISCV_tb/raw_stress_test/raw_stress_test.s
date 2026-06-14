nop
start:
addi x1, x0, 5
addi x2, x1, 10
addi x3, x2, 15
addi x4, x3, 20
addi x5, x4, 25
addi x6, x5, 30
addi x7, x6, 35
addi x8, x7, 40
addi x9, x8, 45
addi x10, x9, 50
addi x11, x10, 55
addi x12, x11, 60
addi x13, x12, 65
addi x14, x13, 70
addi x15, x14, 75

jal x0, chain1

chain1:
add x1, x1, x2
add x2, x1, x2
add x3, x2, x3
add x4, x3, x4
add x5, x4, x5
add x6, x5, x6
add x7, x6, x7
add x8, x7, x8
add x9, x8, x9
add x10, x9, x10
add x11, x10, x11
add x12, x11, x12
add x13, x12, x13
add x14, x13, x14
add x15, x14, x15

jal x0, chain2

chain2:
add x1, x1, x1
add x2, x1, x1
add x3, x2, x2
add x4, x3, x3
add x5, x4, x4
add x6, x5, x5
add x7, x6, x6
add x8, x7, x7
add x9, x8, x8
add x10, x9, x9
add x11, x10, x10
add x12, x11, x11
add x13, x12, x12
add x14, x13, x13
add x15, x14, x14

jal x0, chain3

chain3:
add x1, x2, x3
add x2, x3, x4
add x3, x4, x5
add x4, x5, x6
add x5, x6, x7
add x6, x7, x8
add x7, x8, x9
add x8, x9, x10
add x9, x10, x11
add x10, x11, x12
add x11, x12, x13
add x12, x13, x14
add x13, x14, x15
add x14, x15, x1
add x15, x14, x13

jal x0, chain4

chain4:
add x1, x1, x2
add x2, x1, x3
add x3, x2, x4
add x4, x3, x5
add x5, x4, x6
add x6, x5, x7
add x7, x6, x8
add x8, x7, x9
add x9, x8, x10
add x10, x9, x11
add x11, x10, x12
add x12, x11, x13
add x13, x12, x14
add x14, x13, x15
add x15, x14, x1

jal x0, loop

loop:
add x1, x1, x2
add x2, x1, x3
add x3, x2, x4
add x4, x3, x5
add x5, x4, x6
add x6, x5, x7
add x7, x6, x8
add x8, x7, x9
add x9, x8, x10
add x10, x9, x11
add x11, x10, x12
add x12, x11, x13
add x13, x12, x14
add x14, x13, x15
add x15, x14, x1

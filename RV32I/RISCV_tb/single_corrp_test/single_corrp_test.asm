nop
addi  x2, x0, 10      # x2 = 10

jal   x3, func1   # jump, x3 = return address

addi  x4, x0, 20      # x4 = 20
add   x5, x1, x2      # x5 = 15
add   x6, x4, x5      # x6 = 35

jal   x0, end1

func1:
addi  x7, x0, 100     # x7 = 100
addi  x8, x7, 50      # x8 = 150

end1:
nop

lui   x10, 0x00001
addi  x10, x10, 0x20  

addi  x11, x0, 30     # x11 = 30
addi  x12, x0, 40     # x12 = 40


addi  x14, x0, 50     # x14 = 50
sub   x15, x14, x11   # x15 = 20

jal   x0, end2

func2:
addi  x16, x0, 200    # x16 = 200
addi  x17, x16, 100   # x17 = 300
end2:
nop

# ======================================================
# TEST 3: BRANCH NOT TAKEN (misprediction)
# Static prediction TAKEN, ali branch NOT TAKEN -> FLUSH!
# ======================================================

addi  x20, x0, 100    # x20 = 100
addi  x21, x0, 50     # x21 = 50

# 100 < 50? FALSE - branch NOT TAKEN
blt   x20, x20, target_branch

# Ovo se IZVRŠAVA (branch NOT taken)
addi  x22, x7, 60     # x22 = 60
addi  x23, x8, 10    # x23 = 70
add   x24, x6, x20   # x24 = 170

jal   x0, end3

target_branch:
addi  x7, x0, 170     # x7 = 100
addi  x8, x7, 150      # x8 = 150
lui   x10, 0x00001
addi  x10, x10, 0x20  # 
addi  x14, x0, 50     # x14 = 50
sub   x15, x14, x11   # x15 = 20

addi  x11, x0, 30     # x11 = 30
addi  x12, x0, 40     # x12 = 40
addi  x16, x0, 200    # x16 = 200
addi  x17, x16, 100   # x17 = 300
addi  x25, x0, 999 
addi  x26, x0, 888    
addi  x14, x0, 50     # x14 = 50
sub   x15, x14, x11   # x15 = 20

addi  x11, x0, 30     # x11 = 30
addi  x12, x0, 40     # x12 = 40
addi  x16, x0, 200    # x16 = 200
addi  x17, x16, 100   # x17 = 300
addi  x25, x0, 999    
addi  x26, x0, 888    
end3:
nop

addi  x27, x0, 10     # x27 = 10
addi  x28, x0, 20     # x28 = 20

addi  x29, x0, 555   
addi  x30, x0, 30     # x30 = 30
add   x31, x30, x27   # x31 = 40
addi  x8, x7, 150      # x8 = 150
lui   x10, 0x00001
addi  x10, x10, 0x20  # 
addi  x14, x0, 50     # x14 = 50
sub   x15, x14, x11   # x15 = 20

addi  x11, x0, 30     # x11 = 30
addi  x12, x0, 40     # x12 = 40
addi  x16, x0, 200    # x16 = 200
addi  x17, x16, 100   # x17 = 300
addi  x25, x0, 999 
addi  x26, x0, 888    
addi  x14, x0, 50     # x14 = 50
sub   x15, x14, x11   # x15 = 20

addi  x11, x0, 30     # x11 = 30
addi  x12, x0, 40     # x12 = 40
addi  x16, x0, 200    # x16 = 200
addi  x17, x16, 100   # x17 = 300
addi  x25, x0, 999    
addi  x26, x0, 888    
addi  x4, x0, 20      # x4 = 20
add   x5, x1, x2      # x5 = 15
add   x6, x4, x5      # x6 = 35

lui   x10, 0x00001
addi  x10, x10, 0x20  

addi  x11, x0, 30     # x11 = 30
addi  x12, x0, 40     # x12 = 40


addi  x14, x0, 50     # x14 = 50
sub   x15, x14, x11   # x15 = 20

nop
nop

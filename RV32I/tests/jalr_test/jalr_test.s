nop
# ===================================================================
# JALR TEST
# Every flavour of jalr: call/return, nested calls with the return
# address saved on a stack in memory, calls through a function table
# loaded from memory, a switch through a jump table, and jalr with a
# non-zero offset. JALR is not predicted: every one flushes the
# fall-through at commit. Branches decide which entries get called.
# x1 = return address, x2 = stack pointer
# ===================================================================

# -------------------- INITIALIZATION --------------------------------
addi    x2,  x0, 0x3F0        # stack
addi    x5,  x0, 3
addi    x6,  x0, 7
addi    x31, x0, 0            # checksum

# -------------------- FUNCTIONS (addresses captured with jal) -------
# f_add: x10 = x10 + x11
jal     x20, over_add
f_add:
add     x10, x10, x11
jalr    x0,  x1,  0
over_add:
sw      x20, 0x100(x0)

# f_xor: x10 = x10 xor x11
jal     x20, over_xor
f_xor:
xor     x10, x10, x11
jalr    x0,  x1,  0
over_xor:
sw      x20, 0x104(x0)

# f_shl: x10 = x10 << (x11 & 7)
jal     x20, over_shl
f_shl:
andi    x12, x11, 7
sll     x10, x10, x12
jalr    x0,  x1,  0
over_shl:
sw      x20, 0x108(x0)

# f_outer: nested call, return address saved on the stack
# x10 = f_add(x10, x11) then xor 0x5A
jal     x20, over_outer
f_outer:
addi    x2,  x2,  -4
sw      x1,  0(x2)
jal     x1,  f_add
xori    x10, x10, 0x5A
lw      x1,  0(x2)
addi    x2,  x2,  4
jalr    x0,  x1,  0
over_outer:
sw      x20, 0x10C(x0)

# -------------------- DIRECT CALLS ----------------------------------
addi    x10, x0, 20
addi    x11, x0, 22
jal     x1,  f_add            # 42
add     x31, x31, x10
jal     x1,  f_outer          # (42 + 22) xor 0x5A
add     x31, x31, x10

# -------------------- CALLS THROUGH THE TABLE (loop) ----------------
addi    x21, x0, 0            # index * 4
addi    x22, x0, 16
addi    x10, x0, 1
addi    x11, x0, 3
table_loop:
lw      x23, 0x100(x21)
jalr    x1,  x23, 0           # call through the loaded pointer
add     x31, x31, x10
addi    x21, x21, 4
blt     x21, x22, table_loop

# -------------------- SWITCH ON A COMPUTED INDEX --------------------
# index = (x31 & 3), dispatch through the jump table of case labels
jal     x20, over_cases
case0:
addi    x24, x0, 100
jal     x0,  switch_end
case1:
addi    x24, x0, 101
jal     x0,  switch_end
case2:
addi    x24, x0, 102
jal     x0,  switch_end
case3:
addi    x24, x0, 103
jal     x0,  switch_end
over_cases:
# case labels are 8 bytes apart starting at x20
andi    x25, x31, 3
slli    x26, x25, 3
add     x27, x20, x26
jalr    x0,  x27, 0
addi    x24, x0,  0x7AD       # flushed
switch_end:
add     x31, x31, x24

# -------------------- JALR WITH A NON-ZERO OFFSET -------------------
# base of a second table, entry picked by the immediate
jal     x20, over_tail
tail0:
addi    x28, x0, 0x7AD        # skipped
jal     x0,  tail_end
tail1:
addi    x28, x0, 0x111
jal     x0,  tail_end
over_tail:
jalr    x0,  x20, 8           # tail1
addi    x28, x0, 0x7AE        # flushed
tail_end:
add     x31, x31, x28


# ===================================================================
# CHAOS SECTIONS - each one deliberately aims at one weakness
# ===================================================================
# TARGET: odd jalr target - bit 0 of rs1 + imm must be cleared
jal     x7,  over_odd
odd_tgt:
addi    x8,  x0,  0x31
jal     x0,  odd_done
over_odd:
addi    x9,  x7,  1           # odd address
jalr    x0,  x9,  0
addi    x8,  x0,  0x7AD
odd_done:
add     x31, x31, x8

# TARGET: odd target made by the immediate
jal     x7,  over_odd2
odd_tgt2:
addi    x8,  x0,  0x32
jal     x0,  odd_done2
over_odd2:
jalr    x0,  x7,  1
addi    x8,  x0,  0x7AE
odd_done2:
add     x31, x31, x8

# TARGET: rd == rs1 (the link overwrites the base register)
jal     x10, over_same
same_tgt:
addi    x11, x0,  0x41
jal     x0,  same_done
over_same:
jalr    x10, x10, 0
addi    x11, x0,  0x7AD
same_done:
add     x31, x31, x11
add     x31, x31, x10

# TARGET: negative immediate
jal     x12, over_neg
neg_tgt:
addi    x13, x0,  0x51
jal     x0,  neg_done
over_neg:
addi    x12, x12, 8
jalr    x0,  x12, -8
addi    x13, x0,  0x7AD
neg_done:
add     x31, x31, x13

# TARGET: return chain - jalr right behind jalr (f -> g -> h)
jal     x1,  rc_f
add     x31, x31, x10
jal     x0,  rc_done
rc_f:
addi    x2,  x2,  -4
sw      x1,  0(x2)
jal     x1,  rc_g
lw      x1,  0(x2)
addi    x2,  x2,  4
jalr    x0,  x1,  0
rc_g:
addi    x2,  x2,  -4
sw      x1,  0(x2)
jal     x1,  rc_h
lw      x1,  0(x2)
addi    x2,  x2,  4
jalr    x0,  x1,  0
rc_h:
addi    x10, x0,  0x77
jalr    x0,  x1,  0
rc_done:

# TARGET: recursion on the stack (sum 1..10): store -> load -> jalr
addi    x10, x0,  10
jal     x1,  rsum
add     x31, x31, x10
jal     x0,  rsum_done
rsum:
beq     x10, x0,  rsum_base
addi    x2,  x2,  -8
sw      x1,  0(x2)
sw      x10, 4(x2)
addi    x10, x10, -1
jal     x1,  rsum
lw      x11, 4(x2)
add     x10, x10, x11
lw      x1,  0(x2)
addi    x2,  x2,  8
rsum_base:
jalr    x0,  x1,  0
rsum_done:

# TARGET: jalr right behind the instruction producing its rs1
jal     x14, over_raw
raw_tgt:
addi    x15, x0,  0x61
jal     x0,  raw_done
over_raw:
addi    x16, x14, 0
jalr    x0,  x16, 0
addi    x15, x0,  0x7AD
raw_done:
add     x31, x31, x15

# TARGET: trampoline - jalr whose target is another jalr
jal     x17, over_tramp
tramp:
jalr    x0,  x18, 0
over_tramp:
jal     x18, over_final
final_tgt:
addi    x19, x0,  0x71
jal     x0,  tramp_done
over_final:
jalr    x0,  x17, 0           # -> tramp -> final_tgt
addi    x19, x0,  0x7AD
tramp_done:
add     x31, x31, x19

# TARGET: jalr on the wrong path of a mispredicted branch
addi    x20, x0,  1
beq     x20, x0,  jw_wrong    # not taken
add     x31, x31, x20
jal     x0,  jw_done
jw_wrong:
jalr    x0,  x0,  0           # would restart the program
jw_done:

# -------------------- RESULT ----------------------------------------
lw      x26, 0x100(x0)
lw      x27, 0x10C(x0)
lw      x29, 0x3EC(x0)        # return address left on the stack
add     x30, x31, x24

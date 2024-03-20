# COMP1521 21T2 ... final exam, question 2

	.data
even_parity_str:	.asciiz "the parity is even\n"
odd_parity_str:		.asciiz "the parity is odd\n"

	.text
main:
	li	$v0, 5
	syscall
	move	$t0, $v0
	# input is in $t0

	li	$t1, 0	# bit idx
	li	$t2, 0	# n bits set

while_start:
	beq	$t1, 32, while_end
	
        move    $t3, $t0        # n
        srlv    $t3, $t3, $t1   # n >> bit_idx
        andi	$t4, $t3, 1     # $t4 = $t3 & 1
        
        add     $t2, $t2, $t4
        addi	$t1, $t1, 1     
        
while_end:

if_start:
        li      $t5, 2
        remu    $t6, $t2, $t5

        beq     $t6, 0, if_else

	li	$v0, 4
	la	$a0, odd_parity_str
	syscall

        j       end


if_else:
        li	$v0, 4
	la	$a0, even_parity_str
	syscall

	# TODO

end:

	li	$v0, 0
	jr	$ra

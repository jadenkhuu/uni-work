# COMP1521 22T2 ... final exam, question 2

main:
	li	$v0, 5		# syscall 5: read_int
	syscall
	move	$t0, $v0	# read integer into $t0


	# THESE LINES JUST PRINT 42\n
	# REPLACE THE LINES BELOW WITH YOUR CODE
count_zero_bits:
        li      $t1, 0          # int num zeroes
        li      $t2, 0          # int i
loop_start:
        bge     $t2, 32, loop_end       # end loop if fail

        srav    $t4, $t0, $t2   # x >> i
        li      $t3, 1          # t3 = 1
        and     $t5, $t3, $t4   # t5 = 1 & (x >> i)

        addi    $t2, $t2, 1
if:
        bne     $t5, 0, loop_start
        addi    $t1, $t1, 1
        j       loop_start

loop_end:

	li	$v0, 1		# syscall 1: print_int
	move    $a0, $t1
	syscall			# printf("42");

	li	$v0, 11		# syscall 11: print_char
	li	$a0, '\n'
	syscall			# printf("\n");
	# REPLACE THE LINES ABOVE WITH YOUR CODE

main__end:
	li	$v0, 0		# return 0;
	jr	$ra

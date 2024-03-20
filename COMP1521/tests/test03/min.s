#  print the minimum of two integers
# $t0 = x;
# $t1 = y;
 
main:
	li	$v0, 5		# scanf("%d", &x);
	syscall			#
	move	$t0, $v0

	li	$v0, 5		# scanf("%d", &y);
	syscall			#
	move	$t1, $v0

	blt $t0, $t1, xprint
        bge $t0, $t1, yprint

xprint:        
        li $v0, 1
        move $a0, $t0
        syscall
        li $v0, 11
        li $a0, '\n'
        syscall
        b end
yprint:

        li $v0, 1
        move $a0, $t1
        syscall
        li $v0, 11
        li $a0, '\n'
        syscall
        b end

end:
	li	$v0, 0		# return 0
	jr	$ra

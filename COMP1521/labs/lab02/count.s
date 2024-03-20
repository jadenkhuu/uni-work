# read a number n and print the integers 1..n one per line
#
# Before starting work on this task, make sure you set your tab-width to 8!
# It is also suggested to indent with tabs only.
#
# Jaden Khuu, 25/09/2022

#![tabsize(8)]

main:                 		# int main(void)
	la	$a0, prompt	# printf("Enter a number: ");
	li	$v0, 4
	syscall

	li	$v0, 5		# scanf("%d", number);
	syscall
        move    $t1, $v0
        
        li      $t0, 1

        b       loop

loop:
        bgt     $t0, $t1, end

        move    $a0, $t0        # print int
        li      $v0, 1
        syscall

        li      $a0, '\n'       # print \n
        li      $v0, 11
        syscall

        addi	$t0, $t0, 1     # $t0 = $t0 + 1 

        b       loop            #goto loop

end:
	li	$v0, 0
	jr	$ra		# return 0

	.data
prompt:
	.asciiz "Enter a number: "

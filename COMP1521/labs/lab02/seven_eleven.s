# Read a number and print positive multiples of 7 or 11 < n
#
# Before starting work on this task, make sure you set your tab-width to 8!
# It is also suggested to indent with tabs only.
#
# Jaden Khuu, 25/09/2022

#![tabsize(8)]

main:				# int main(void) {

	la	$a0, prompt	# printf("Enter a number: ");
	li	$v0, 4
	syscall

	li	$v0, 5		# scanf("%d", number);
	syscall
        move    $t1, $v0

	li      $t0, 1

        b       loop

loop:
        beq     $t0, $t1, end   # t0 > t1 
        
        rem     $t2, $t0, 7     # multiple of 7
        beq     $t2, 0, print

        rem     $t2, $t0, 11    # multiple of 11
        beq     $t2, 0, print

        b       increment

print:
        move    $a0, $t0
        li      $v0, 1
        syscall

        li	$a0, '\n'	# printf("%c", '\n');
	li	$v0, 11
        syscall 

        b       increment

increment: 
        addi    $t0, $t0, 1     # i++
        b       loop

end:
	li	$v0, 0
	jr	$ra		# return 0

	.data
prompt:
	.asciiz "Enter a number: "

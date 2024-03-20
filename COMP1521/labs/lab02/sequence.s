# Read three numbers `start`, `stop`, `step`
# Print the integers bwtween `start` and `stop` moving in increments of size `step`
#
# Before starting work on this task, make sure you set your tab-width to 8!
# It is also suggested to indent with tabs only.
#
# YOUR-NAME-HERE, DD/MM/YYYY

#![tabsize(8)]

main:				# int main(void)
	la	$a0, prompt1	# printf("Enter the starting number: "); t0
	li	$v0, 4
	syscall

	li	$v0, 5		# scanf("%d", number);
	syscall
        move    $t0, $v0

        la	$a0, prompt2	# printf("Enter the stopping number: "); t1 
	li	$v0, 4
	syscall

	li	$v0, 5		# scanf("%d", number);
	syscall
        move    $t1, $v0

        la	$a0, prompt3	# printf("Enter the step size: "); t2 
	li	$v0, 4
	syscall

	li	$v0, 5		# scanf("%d", number);
	syscall
        move    $t2, $v0

        move    $t9, $t0        # int i = start

        bge     $t1, $t0, loop_end      # if (stop >= start) goto loop_end;
        bge     $t2, $0, loop_end       # if (step >= 0) goto loop_end;


loop_forward:
        blt     $t9, $t1, loop_end      # if (i < stop) goto loop_end;

        move    $a0, $t9                # print int
        li      $v0, 1
        syscall

        li      $a0, '\n'       # print \n
        li      $v0, 11
        syscall

        add     $t9, $t9, $t2           # i = i + step;
        b       loop_forward

loop_end: 
        ble     $t1, $t0, end
        ble     $t2, $0, end

loop2_start:
        bgt     $t9, $t1, end
        
        move    $a0, $t9                # print int
        li      $v0, 1
        syscall

        li      $a0, '\n'       # print \n
        li      $v0, 11
        syscall

        add     $t9, $t9, $t2  

        b       loop2_start        

end:
	li	$v0, 0
	jr	$ra		        # return 0


	.data
prompt1:
	.asciiz "Enter the starting number: "
prompt2:
	.asciiz "Enter the stopping number: "
prompt3:
	.asciiz "Enter the step size: "

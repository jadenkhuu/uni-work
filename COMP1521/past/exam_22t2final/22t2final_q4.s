	.data
numbers:
	.word 0:10	# int numbers[10] = { 0 };

	.text
main:
	li	$t0, 0		# i = 0;

main__input_loop:
	bge	$t0, 10, main__input_finished	# while (i < 10) {

	li	$v0, 5			# syscall 5: read_int
	syscall
	mul	$t1, $t0, 4
	sw	$v0, numbers($t1)	#	scanf("%d", &numbers[i]);
	
	addi	$t0, 1			#	i++;
	b	main__input_loop	# }

main__input_finished:
	#############################
	# TODO: YOUR CODE GOES HERE #
	#############################

	li	$t0, 1          # i = 0
	li      $t1, 1          # max run = 0
        li      $t2, 1          # current_run = 0

count_loop_start:
        bge     $t0, 10, count_loop_end         # while (i < 10)

first_if:
        mul     $t6, $t0, 4
        lw      $t3, numbers($t6)       # numbers[i]
        sub     $t4, $t0, 1     
        mul     $t6, $t4, 4
        lw      $t5, numbers($t6)       # numbers[i - 1]

        ble     $t3, $t5, first_if_else # if (numbers[i] > numbers[i - 1]) 
        addi    $t2, $t2, 1             # current_run++;
        j       second_if_start

first_if_else:
        li      $t2, 1                  # current_run = 1;

second_if_start:
        ble     $t2, $t1, second_if_end
        move    $t1, $t2        # max_run = current_run

second_if_end:
        addi    $t0, $t0, 1
        j       count_loop_start

count_loop_end:

	li	$v0, 1		# syscall 1: print_int
	move	$a0, $t1
	syscall			# printf("max run");

	li	$v0, 11		# syscall 11: print_char
	li	$a0, '\n'
	syscall			# printf("\n");

	li	$v0, 0
	jr	$ra		# return 0;

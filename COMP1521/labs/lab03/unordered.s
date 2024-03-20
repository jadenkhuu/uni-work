# Reads 10 numbers into an array
# printing 0 if they are in non-decreasing order
# or 1 otherwise.
# YOUR-NAME-HERE, DD/MM/YYYY

# Constants
ARRAY_LEN = 10

main:
	# Registers:
	#  - $t0: int i
	#  - $t1: temporary result
	#  - $t2: temporary result
	#  TODO: add your registers here

	# $t3: swap int
	# $t4: int x
	# $t5: int y

scan_loop__init:
	li	$t0, 0				# i = 0;
scan_loop__cond:
	bge	$t0, ARRAY_LEN, scan_loop__initialise	# while (i < ARRAY_LEN) {

scan_loop__body:
	li	$v0, 5				#   syscall 5: read_int
	syscall					#   
						#
	mul	$t1, $t0, 4			#   calculate &numbers[i] == numbers + 4 * i
	la	$t2, numbers			#
	add	$t2, $t2, $t1			#
	sw	$v0, ($t2)			#   scanf("%d", &numbers[i]);

	addi	$t0, $t0, 1			#   i++;
	j	scan_loop__cond			# }

scan_loop__initialise:
        li      $t0,    1
        li      $t3,    0


scan_loop__second_body:
        bge     $t0, ARRAY_LEN, scan_loop__end
        
        mul     $t1, $t0, 4
        la      $t2, numbers
        add     $t2, $t2, $t1
        lw      $t4, ($t2)
        sub     $t2, $t2, 4
        lw      $t5, ($t2)
        blt     $t4, $t5, scan_loop__if

        addi    $t0, $t0, 1
        b       scan_loop__second_body

scan_loop__if:
        li      $t3, 1
        addi    $t0, $t0, 1
        b       scan_loop__second_body

scan_loop__end:

        move    $a0, $t3
	li	$v0, 1				# syscall 1: print_int	
	syscall					# printf("%d", 42)

	li	$v0, 11				# syscall 11: print_char
	li	$a0, '\n'			#
	syscall					# printf("%c", '\n');

	li	$v0, 0
	jr	$ra				# return 0;

	.data
numbers:
	.word	0:ARRAY_LEN			# int numbers[ARRAY_LEN] = {0};

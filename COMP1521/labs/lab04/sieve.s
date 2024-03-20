# Sieve of Eratosthenes
# https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
# YOUR-NAME-HERE, DD/MM/YYYY

# Constants

# $t0, i
# $t1, j 
# $t6, 0

ARRAY_LEN = 1000

main:
	li $t0, 2					# int i = 2
	li $t6, 0
first_loop: 
	bge $t0, ARRAY_LEN, end_1	# for (int i = 2; i < ARRAY_LEN; i++)

statement: 
	la $t3, prime
	add $t3, $t3, $t0
	lb $t4, ($t3)				# load prime[i] into $t4 

	beq $t4, 0, end	# if (prime[i] != 0) 

	move $a0, $t0
	li $v0, 1
	syscall						# printf("%d", i);

	li $a0, '\n'				# printf("\n", i);
	li $v0, 11
	syscall

	mul $t1, $t0, 2				# int j = 2 * i 

second_loop:

	bge $t1, ARRAY_LEN, end	# for (int j = 2 * i; j < ARRAY_LEN; j += i)

	la $t3, prime
	add $t3, $t3, $t1
	sb $t6, ($t3)					# prime[j] = 0 

	add $t1, $t1, $t0 	# j += i
	j second_loop

end: 

	addi $t0, $t0, 1		# i++
	j first_loop

end_1:

	li	$v0, 0
	jr	$ra			# return 0;

	.data
prime:
	.byte	1:ARRAY_LEN		# uint8_t prime[ARRAY_LEN] = {1, 1, ...};
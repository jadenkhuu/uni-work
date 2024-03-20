# Reads a 4-byte value and reverses the byte order, then prints it

########################################################################
# .TEXT <main>
main:

	li      $v0, 5          # scanf("%d", &x);
	syscall
	move    $t0, $v0        # $t1 = network_bytes

	li      $t1, 0          # int32_t computer_bytes = 0;
	la      $t2, 0xFF       # uint32_t byte_mask = BYTE_MASK

	and     $t3, $t0, $t2   # network_bytes & byte_mask
	sll     $t3, $t3, 24    # (network_bytes & byte_mask) << 24
	or      $t1, $t1, $t3   # computer_bytes |= (network_bytes & byte_mask) << 24

	sll     $t3, $t2, 8     # byte_mask << 8
	and     $t3, $t3, $t0     # network_bytes & (byte_mask << 8)
	sll     $t3, $t3, 8     # (network_bytes & (byte_mask << 8)) << 8
	or      $t1, $t1, $t3     # computer_bytes |= (network_bytes & (byte_mask << 8)) << 8

	sll     $t3, $t2, 16    # byte_mask << 16
	and     $t3, $t3, $t0   # network_bytes & (byte_mask << 16)
	srl     $t3, $t3, 8     # (network_bytes & (byte_mask << 16)) >> 8
	or      $t1, $t1, $t3   # computer_bytes |= (network_bytes & (byte_mask << 16)) >> 8

	sll     $t3, $t2, 24    # byte_mask << 24
	and     $t3, $t3, $t0   # network_bytes & (byte_mask << 24)
	srl     $t3, $t3, 24    # (network_bytes & (byte_mask << 24)) >> 24
	or      $t1, $t1, $t3   # computer_bytes |= (network_bytes & (byte_mask << 24)) >> 24

        move    $a0, $t1        # printf("%d\n", x);
        li      $v0, 1
        syscall

	li	$a0, '\n'	# printf("%c", '\n');
	li	$v0, 11
	syscall

main__end:
	li	$v0, 0		# return 0;
	jr	$ra

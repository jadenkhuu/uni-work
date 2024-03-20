# read a mark and print the corresponding UNSW grade
#
# Before starting work on this task, make sure you set your tab-width to 8!
# It is also suggested to indent with tabs only.
#
# Jaden Khuu, 25/09/2022

#![tabsize(8)]

main:
	la	$a0, prompt	# printf("Enter a mark: ");
	li	$v0, 4
	syscall

	li	$v0, 5		# scanf("%d", mark);
	syscall
        move    $t0, $v0

        bgt     50, $t0, failed  # if $t0 > $t1 then target
        bgt     65, $t0, pass
        bgt     75, $t0, credit
        bgt     85, $t0, dist

        la	$a0, hd	        # printf("HD\n");
        li	$v0, 4
        syscall
        b       end

failed:
        la	$a0, fl		# printf("FL\n");
	li	$v0, 4
	syscall
        b       end
pass: 
        la	$a0, ps	        # printf("PS\n");
	li	$v0, 4
	syscall
        b       end

credit:
        la	$a0, cr 	# printf("CR\n");
	li	$v0, 4
	syscall
        b       end

dist:
        la	$a0, dn		# printf("DN\n");
	li	$v0, 4
	syscall
        b       end

end: 
	li	$v0, 0
	jr	$ra		# return 0


	.data
prompt:
 	.asciiz "Enter a mark: "
fl:
	.asciiz "FL\n"
ps:
	.asciiz "PS\n"
cr:
	.asciiz "CR\n"
dn:
	.asciiz "DN\n"
hd:
	.asciiz "HD\n"

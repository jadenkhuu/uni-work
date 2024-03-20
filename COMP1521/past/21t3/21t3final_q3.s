# read an integer
# print 1 iff  the least significant bit is equal to the most significant bit
# print 0 otherwise

main:
        li      $v0, 5
        syscall
        move    $t0, $v0

    # THESE LINES JUST PRINT 42
    # REPLACE THE LINES BELOW WITH YOUR CODE

        li $t1, 1       
        and $t3, $t0, $t1 

        srl $t2, $t1, 31
        and $t4, $t2, $t1
    
        bge $t0, 0, if_statement

        li $t4, 1

if_statement:
        bne     $t3, $t4, print_diff

print_same:
        li      $v0, 1
        li      $a0, 1
        syscall
        
        li $v0, 11
        li $a0, '\n'
        syscall 
        
        j       end

print_diff:
        li      $v0, 1
        li      $a0, 0
        syscall

        li $v0, 11
        li $a0, '\n'
        syscall 

        j       end

end:
        li $v0, 0
        jr $31

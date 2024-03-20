# read two integers and print all the integers which have their bottom 2 bits set.

main:
        li      $v0, 5
        syscall
        move    $t0, $v0        # scan x

        li      $v0, 5     
        syscall
        move    $t1, $v0        # scan y


        # THESE LINES JUST PRINT 42
        # REPLACE THE LINES BELOW WITH YOUR CODE
        
        addi    $t0, $t0, 1

while_start:
        bge     $t0, $t1, while_end

if_start: 
        andi    $t3, $t0, 1     # (x & 1) = $t3
        bne     $t3, 1, if_end  # ((x & 1) == 1)

        li      $t4, 1
        sll     $t4, $t4, 1     # (1 << 1)
        and     $t3, $t0, $t4   # (x & (1 << 1))
        bne     $t3, $t4, if_end  # (x & (1 << 1)) == (1 << 1)

        li      $v0, 1
        move    $a0, $t0
        syscall                 # print x

        li      $v0, 11
        li      $a0, '\n'
        syscall
        # REPLACE THE LINES ABOVE WITH YOUR CODE

if_end:
        addi    $t0, $t0, 1
        j       while_start

while_end:

end:
        li      $v0, 0
        jr      $31

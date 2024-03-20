 
# $t0 = x
# $t1 = y
# $t2 = i


main:
        li $v0, 5
        syscall
        move $t0, $v0
       
        li $v0, 5
        syscall
        move $t1, $v0
       
        addi $t2, $t0, 1
loop:
        bge $t2, $t1, end      
       
       
        beq $t2, 13, increment
        li $v0, 1
        move $a0, $t2
        syscall
       
        li $v0, 11
        li $a0, '\n'
        syscall
       
increment:
        addi $t2, $t2, 1
        b loop
end:
        li $v0, 0
        jr $ra  
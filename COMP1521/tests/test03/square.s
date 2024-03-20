# x = $t0
# i = $t 1
# j = $t2

main:  
        li $v0, 5
        syscall
        move $t0, $v0
       
        li $t1, 0
       
first_loop:
        bge $t1, $t0, end
        li $t2, 0

second_loop:
        bge $t2, $t0, increment
        
        li $v0, 11
        li $a0, '*'     # print *
        syscall
       
        addi $t2, $t2, 1
       
        b second_loop


increment:
        addi $t1, $t1, 1        # increment 
       
        li $v0, 11
        li $a0, '\n'    # print new line
        syscall
       
        b first_loop
end:
        li $v0, 0
        jr $ra
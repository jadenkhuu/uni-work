 # read 10 numbers into an array

main:

        li      $t0, 0          # i = 0
loop0:
        bge     $t0, 10, end0   # while (i < 10) {

        li      $v0, 5          #   scanf("%d", &numbers[i]);
        syscall              #

        mul     $t1, $t0, 4     #   calculate &numbers[i]
        la      $t2, numbers    #
        add     $t3, $t1, $t2   #
        sw      $v0, ($t3)      #   store entered number in array

        add     $t0, $t0, 1     #   i++;
        b       loop0           # }
end0:

    # ADD YOUR CODE HERE
    # ADD YOUR CODE HERE
        li      $t0, 0          # i = 0

loop1:
        bge     $t0, 10, end1   # while (i < 10) {

if_start_1:
        mul     $t8, $t0, 4
        lw      $t9, numbers($t8)
        ble     $t9, 0, if_not_1

        li      $v0, 1
        move    $a0, $t9
        syscall
        li $v0, 11
        li $a0, ' '
        syscall 

if_not_1:
        addi    $t0, $t0, 1
        j       loop1


end1:
        li $v0, 11
        li $a0, '\n'
        syscall 

        li      $t0, 0          # i = 0

loop2:
        bge     $t0, 10, end2   # while (i < 10) {

if_start_2:
        mul     $t8, $t0, 4
        lw      $t9, numbers($t8)
        bge     $t9, 0, if_not_2
        
        li      $v0, 1
        move    $a0, $t9
        syscall
        li $v0, 11
        li $a0, ' '
        syscall 

if_not_2:
        addi    $t0, $t0, 1
        j       loop2

end2:
        li $v0, 11
        li $a0, '\n'
        syscall 

        li      $v0, 0
        jr      $31             # return

.data

numbers:
        .word 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  # int numbers[10] = {0};

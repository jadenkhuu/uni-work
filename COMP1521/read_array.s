        N_SIZE = 10

main:
        # $t0: int i
        # $t1: int num


main__read_arr:

main__read_arr__init:
        li      $t0, 0          # i = 0
        
main__read_arr__cond:
        bge     $t0, N_SIZE, main__read_arr__end

main__read_arr__body:
        li      $v0, 5          # syscall 5: read_int
        syscall                 # scanf int
        move    $t1, $v0

        la      $t2, numbers    # &element == base + index + sizeof(element)
        mul     $t3, $t0, 4
        add     $t2, $t2, $t3

        sw      $t1, ($t2)      # numbers[i] = num

main__read_arr__step:
        addi	$t0, $t0, 1	# $t0 = $01 1 0
        b       main__read_arr__cond

main__read_arr__end:
        li      $v0, 0
        jr      $ra 
        
        

        .data
numbers: 
        .space N_SIZE * 4
        
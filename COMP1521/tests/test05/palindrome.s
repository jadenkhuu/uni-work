# Reads a line and prints whether it is a palindrome or not

LINE_LEN = 256

########################################################################
# .TEXT <main>
main:
        # Locals:
        #   - ...
 
        li      $v0, 4                          # syscall 4: print_string
        la      $a0, line_prompt_str            #
        syscall                                 # printf("Enter a line of input: ");
 
        li      $v0, 8                          # syscall 8: read_string
        la      $a0, line                       #
        la      $a1, LINE_LEN                   #
        syscall                                 # fgets(buffer, LINE_LEN, stdin)
 
       
        li $t0, 0
 
     
 
loop:
        la $t1, line
        mul $t2, $t0, 1
        add $t3, $t2, $t1
 
        lb  $t4, ($t3)
 
 
        beq $t4, 0, next
        addi $t0, $t0, 1
        j loop
next:
        li $t5, 0
 
        move $t6, $t0               # k
        sub $t6, $t6, 2
 
loop_two:
        bge $t5, $t6, result_palindrome
 
        mul $t2, $t5, 1
        add $t3, $t2, $t1
 
        lb $t7, ($t3)
 
        mul $t4, $t6, 1
        add $t9, $t4, $t1
       
        lb $t0, ($t9)
 
        bne $t0, $t7, result_not_palindrome
 
        sub $t6, $t6, 1
        addi $t5, $t5, 1
        j loop_two
 
result_not_palindrome:
        li      $v0, 4                          # syscall 4: print_string
        la      $a0, result_not_palindrome_str  #
        syscall                                 # printf("not palindrome\n");
       
        j end
result_palindrome:
        li      $v0, 4                          # syscall 4: print_string
        la      $a0, result_palindrome_str      #
        syscall                                 # printf("palindrome\n");
 
end:
        li      $v0, 0
        jr      $ra    				# return 0;


########################################################################
# .DATA
	.data
# String literals
line_prompt_str:
	.asciiz	"Enter a line of input: "
result_not_palindrome_str:
	.asciiz	"not palindrome\n"
result_palindrome_str:
	.asciiz	"palindrome\n"

# Line of input stored here
line:
	.space	LINE_LEN


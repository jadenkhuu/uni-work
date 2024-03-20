#
# COMP1521 lab exercise sample solution
#
# A simple MIPS program that calculates the Gaussian sum between two numbers
# Written 12/2/2022
# by Dylan Brotherston (d.brotherston@unsw.edu.au)

# int main(void)
# {
#   int number1, number2;
#
#   printf("Enter first number: ");
#   scanf("%d", &number1);
#
#   printf("Enter second number: ");
#   scanf("%d", &number2);
#
#   int gaussian_sum = ((number2 - number1 + 1) * (number1 + number2)) / 2;
#
#   printf("The sum of all numbers between %d and %d (inclusive) is: %d\n", number1, number2, gaussian_sum);
#
#   return 0;
# }

main:

  #
  # TODO: add your code HERE
  la $a0, prompt1   #printf("Enter first number: ");
  li $v0, 4
  syscall

  li $v0, 5         #scanf("%d", &number1);
  syscall
  move $t0, $v0
  
  la $a0, prompt2   #printf("Enter second number: ");
  li $v0, 4
  syscall

  li $v0, 5         #scanf("%d", &number1);
  syscall
  move $t1, $v0
  
  sub $t2, $t1, $t0 # (number2 - number1)
  add $t3, $t2, 1   # (number2 - number1 + 1)
  add $t4, $t1, $t0 # (number1 + number2)
  mul $t5, $t3, $t4 # (number2 - number1 + 1) * (number1 + number2))
  div $t6, $t5, 2   # (number2 - number1 + 1) * (number1 + number2)) / 2


  la $a0, answer1
  li $v0, 4
  syscall

  li $v0, 1
  move $a0, $t0
  syscall

  la $a0, answer2
  li $v0, 4
  syscall

  li $v0, 1
  move $a0, $t1
  syscall

  la $a0, answer3
  li $v0, 4
  syscall

  li $v0, 1
  move $a0, $t6
  syscall

  li $v0,  11
  li $a0, '\n'
  syscall

  li   $v0, 0
  jr   $ra          # return


.data
  prompt1: .asciiz "Enter first number: "
  prompt2: .asciiz "Enter second number: "

  answer1: .asciiz "The sum of all numbers between "
  answer2: .asciiz " and "
  answer3: .asciiz " (inclusive) is: "

#!/bin/dash

# ==============================================================================
# test_accessing.sh
# 
# testing $ accessing variables
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

# subset 0 examples
P1=race
P2=car
palindrome=$P1$P2
echo $palindrome

theAnswer=42
echo The meaning of life, the universe, and everything is $theAnswer
echo $theAnswer, is a 2 digit number

name=COMP2041
echo I hope you are enjoying $name this semester

H=Hello
W=World
echo $H, $W

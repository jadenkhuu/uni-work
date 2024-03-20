#!/bin/dash

# ==============================================================================
# test_var.sh
# 
# Variable assign testing
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

# '=' examples - Subset 0
x=1
y=2
foo=hello
bar=world
course_code=COMP2041
AssignmentName=Sheepy

name=COMP2041
echo I hope you are enjoying $name this semester

H=Hello
W=World
echo $H, $W

P1=race
P2=car
palindrome=$P1$P2
echo $palindrome
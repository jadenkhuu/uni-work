#!/bin/dash

# ==============================================================================
# demo00.sh
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

echo Hello there
echo What is your name?
read $name

echo Hello $name

echo I will count to 10
for i in 1 2 3 4 5 6 7 8 9 10
do
    echo $i
done

# From subset 0 testing
P1=race
P2=car
palindrome=$P1$P2
echo $palindrome

# This is a comment

echo Program Finished
exit 0
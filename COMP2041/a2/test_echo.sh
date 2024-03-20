#!/bin/dash

# ==============================================================================
# test_echo.sh
# 
# Testing echo function
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

echo hello world
echo 42 is the meaning of life, the universe, and everything
echo To be or not to be: that is the question

echo 'hello    world'

echo 'This is not a $variable'

echo 'This is not a glob *.sh'

echo This   is the   way the world ends
echo This is    the way   the world ends
echo This is the    way    the world       ends
echo Not with a     bang but            a        whimper.

name=Jaden
colour=Purple
echo Hello $name, my favourite colour is $colour too.
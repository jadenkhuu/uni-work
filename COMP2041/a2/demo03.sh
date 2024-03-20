#!/bin/dash

# ==============================================================================
# demo03.sh
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

# Subset 0 examples
echo hello world
echo 42 is the meaning of life, the universe, and everything
echo To be or not to be: that is the question

echo we will now demonstrate if statements and the test function
# examples/2/filetest1.sh
if test -d /dev/null
then
    echo /dev/null
fi

if test -d /dev
then
    echo /dev
fi

# examples/2/truth2.sh
echo 'When old age shall this generation waste,'
echo 'Thou shalt remain, in midst of other woe'
echo 'Than ours, a friend to man, to whom thou sayst,'
echo '"Beauty is truth, truth beauty",  -  that is all'
echo 'Ye know on earth, and all ye need to know.'
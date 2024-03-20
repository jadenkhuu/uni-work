#!/bin/dash

# ==============================================================================
# demo02.sh
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

echo These are some examples of for loops

# examples/1/for_read0.sh	
for n in one two three
do
    echo Line $n
done

# Subset 1 examples
for file in *
do
    echo $file
done

# examples/1/for_exit.sh
for word in Houston 1202 alarm # Comment inline - for loop
do
    echo $word
done

# This is a comment
# This is a comment # this is not a comment

echo Program Finished # This is a comment # this is not a comment
exit 0
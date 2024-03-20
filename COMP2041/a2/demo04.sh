#!/bin/dash

# ==============================================================================
# demo04.sh
# testing cd, read, and external commends
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

# Read an input
echo Choose a name for your Playlist
read name

# if statement
if test $name == ""
then
    echo Your playlist name is empty
    exit 0

else 
    # Print out a list as user inputs
    echo Enter 5 songs
    for n in 1 2 3 4 5
    do
        read input
        echo Song $n: $input
    done
exit 0
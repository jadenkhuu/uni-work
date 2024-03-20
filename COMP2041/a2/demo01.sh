#!/bin/dash

# ==============================================================================
# demo01.sh
# 
# Written by: Jaden Khuu 
# For COMP2041/9044 Assignment 2
# ==============================================================================

# variable assignment
intro="Welcome to this program"

# check command line args
if test "$1" != ""
then
    name=$1
else
    name="unnamed"
fi

# echo assignment variables
echo $intro, $name

# input ()
echo What is your favourite colour?
read colour

echo My favourite colour is also $colour

echo Program Finished
exit 0
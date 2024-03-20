#!/bin/dash

# ==============================================================================
# test03.sh
# Testing $ accessing variables
#
# Written by: Jaden Khuu
# For COMP2041/9044 Assignment 2
# ==============================================================================

PATH="$PATH:$(pwd)"

python3 sheepy.py test_accessing.sh > temp.py; python3 temp.py > actual
./test_accessing.sh > expected

if ! diff -q expected actual >/dev/null 2>&1; then
    echo 'Test failed'
else
    echo 'Test passed'
fi

rm temp.py
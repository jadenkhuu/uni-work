#!/bin/bash

# Starting test number
test_num=1

# Ending test number, note we are given 6 total tests 
end_test=6

# Update program if any changes were made
make

# Loop through all given tests
while [ $test_num -le $end_test ]
do
    ./testGraphMST < tests/graphMST/${test_num}.in >tests/graphMST/${test_num}.out 
    if cmp -s tests/graphMST/${test_num}.out tests/graphMST/${test_num}.exp
    then 
        echo "You passed test ${test_num}"
    else
        echo "For test ${test_num}, your program outputted:"
        cat tests/graphMST/${test_num}.out

        # Empty lines between the two sets of output
        echo ""
        echo "--------------------------------------------"
        echo ""

        echo "while the correct output is:"
        cat tests/graphMST/${test_num}.exp
        
        # Add space between different test cases
        echo ""
    fi

    # Move onto the next test 
    test_num=$((test_num + 1))
done

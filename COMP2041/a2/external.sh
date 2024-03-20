#!/bin/dash

touch test_file.txt
ls -l test_file.txt

for course in COMP1511 COMP1521 COMP2511 COMP2521 # keyword
do                                                # keyword
    echo $course                                  # builtin
    mkdir $course                                 # external command
    chmod 700 $course                             # external command
done        

cd /tmp
pwd

echo This program is: $0

file_name=$2
number_of_lines=$5

echo Going to print the first $number_of_lines lines of $file_name

string=BAR
echo FOO${string}BAZ
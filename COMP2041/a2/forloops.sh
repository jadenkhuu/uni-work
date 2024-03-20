#!/bin/dash

string=BAR
echo FOO${string}BAZ

C_files=*.[ch]
echo $C_files

echo all of the single letter Python files are: ?.py

name=COMP2041
echo I hope you are enjoying $name this semester

for file in *.c
do
    echo $file
done

for word in Houston 1202 alarm
do
    echo $word
    exit 0
done

cd /tmp
cd ..

echo What is your name:
read name

echo What is your favourite colour:
read colour

echo Hello $name, my favourite colour is $colour too.

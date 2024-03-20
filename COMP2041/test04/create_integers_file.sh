#!/bin/dash

x="$1"

while [ "$x" -le "$2" ]
do
    echo "$x" >> "$3"
    x=$((x+1))
done

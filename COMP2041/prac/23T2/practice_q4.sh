#!/bin/dash

lowest=$(grep -E '[0-9]+' $1 | sort | head -n1)
highest=$(grep -E '[0-9]+' $1 | sort -r | head -n1)

i="$l"
while [ "$i" -lt "$highest" ]
do 
    output=$(grep -E "^$i$" $1)
    
    if [ ! "$output" ]
    then
        echo $i
    fi
    i=$((i + 1))

done
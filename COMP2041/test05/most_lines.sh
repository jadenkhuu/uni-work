#!/bin/dash

most=$1
for file in "$@"
do
    mostLines=$(wc -l < "$most")
    if [ -f "$file" ]
    then
        numLines=$(wc -l < "$file")
        if [ "$numLines" -ge "$mostLines" ]
        then
            most="$file"
        fi
    fi
done
echo "$most"
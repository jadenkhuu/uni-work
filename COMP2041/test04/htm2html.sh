#!/bin/dash

for file in *.htm
do
    new=$(echo "$file" | sed -E "s/\.htm$/.html/g")
    
    if test -f "$new"
    then
        echo "$new exists"
        exit 1
    else
        cp "$file" "$new"
        rm "$file"
    fi
done

exit 0
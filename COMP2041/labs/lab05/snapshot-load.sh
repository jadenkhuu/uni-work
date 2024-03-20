#!/bin/dash

number=$1

run="snapshot-save.sh"

"$run"

if ! [ -d "snapshot.$number" ]
then
    for file in ".snapshot.$number"/*
    do
        if ! [ -f "$file" ]
        then
            :
        fi

        if [ "$file" != "snapshot-load.sh" ] && [ "$file" != "snapshot-save.sh" ]
        then
            cp "$file" .
        fi  
    done
fi

echo "Restoring snapshot $number"
#!/bin/dash

if ! [ "$#" -eq 1 ]
then
    echo "Must have 1 argument"
    exit
fi

for file in "$@"
do
    if ! [ -f "$file" ]
    then
        echo "$file does not exist"
    fi

    number=0
    while [ -f ".$file.$number" ] 
    do
        number=$((number+1))
    done

    cp "$file" ".$file.$number"
    echo "Backup of '$file' saved as '.$file.$number'"

done

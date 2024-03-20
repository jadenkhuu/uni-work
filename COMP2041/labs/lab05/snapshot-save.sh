#!/bin/dash

number=0
while [ -d ".snapshot.$number" ]
do
    number=$((number+1))
done

directory=".snapshot.$number"

mkdir "$directory"

for file in *
do
    if ! [ -f "$file" ]
    then
        :
    fi

    if [ "$file" != "snapshot-save.sh" ] && [ "$file" != "snapshot-load.sh" ] && [ "${file%%.*}" != "." ]
    then
        cp "$file" "$directory"
    fi
done

echo "Creating snapshot $number"
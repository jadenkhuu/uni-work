#!/bin/dash

touch sorting.txt

if [ "$(ls -A "${PWD}/${1}")" ]; then
    for file1 in "$1"/*; do

        name1=$( echo "$file1" | sed -E 's/^.*\///g')
        if [ "$(ls -A "${PWD}/${2}")" ]; then
            for file2 in "$2"/*; do 
                name2=$( echo "$file2" | sed -E 's/^.*\///g')
                if [ "$name1" = "$name2" ]; then
                    if diff "${1}/${name1}" "${2}/${name1}" >/dev/null; then
                        echo "$name1" >> sorting.txt
                    fi
                fi

            done 
        fi
    done 
fi

sort sorting.txt
rm sorting.txt
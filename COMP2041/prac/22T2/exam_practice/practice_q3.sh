#!/bin/dash

# for d in *; do
#     test -d "$d" &&
#     test "$(ls -1 "$d" | wc -l)" -ge 2 &&
#     echo "$d"
# done

for dir in *
do
    if test -d "$dir"
    then
        if test "$(ls -1 "$dir" | wc -l)" -ge 2
        then
            echo "$dir"
        fi
    fi
done
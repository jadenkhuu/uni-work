#!/bin/dash

for file in "$@"
do
    include=$(grep -E "#include '.*'" "$file")
    echo "$include" | while IFS= read -r text
    do
        text=$(echo "$text" | cut -d'"' -f2)
        if ! test -f "$text"
        then
            echo "$text included into $file does not exist"
        fi
    done
done
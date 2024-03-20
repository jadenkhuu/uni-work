#!/bin/dash

prefix=$(echo "$1" | sed -E 's/([a-zA-Z]*)[0-9]+[a-zA-Z]*/\1/g')
suffix=$(echo "$1" | sed -E 's/([a-zA-Z]*)[0-9]+([a-zA-Z]*)/\2/g')

low=$(echo "$1" | sed -E 's/[a-zA-Z]*([0-9]+)[a-zA-Z]*/\1/g')
high=$(echo "$2" | sed -E 's/[a-zA-Z]*([0-9]+)[a-zA-Z]*/\1/g')

i="$low"

while [ "$i" -le "$high" ]
do
    echo "${prefix}${i}${suffix}"
    i=$((i + 1))
done
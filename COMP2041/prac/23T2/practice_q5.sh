#!/bin/dash

# awards=$(grep -E "$1" $2)

# low=$(grep -E "^$1\|" "$2" | cut -d'|' -f2 | sort -n | head -n1)
# high=$(grep -E "^$1\|" "$2" | cut -d'|' -f2 | sort -nr | head -n1)

# if [ ! "$high" ]
# then
#     echo "No award matching '$1'"
#     exit
# fi

# i=$low
# while [ "$i" -lt "$high" ]
# do
#     year=$(grep -E "^$1\|" "$2" | cut -d'|' -f2 | grep -E "$i")
#     if [ ! "$year" ]
#     then
#         echo "$i"
#     fi
#     i=$((i + 1))
# done

low=$(grep -E "^$1\|" "$2" | cut -d'|' -f2 | sort -n | head -n1)
high=$(grep -E "^$1\|" "$2" | cut -d'|' -f2 | sort -nr | head -n1)

if ! test "$low" 
then
    echo "No award matching '$1'"
    exit
fi

i=$low
while test "$i" -lt "$high"
do
    year=$(grep -E "^$1\|" "$2" | cut -d'|' -f2 | grep -E "$i")
    if [ ! "$year" ]
    then
        echo "$i"
    fi
    i=$((i + 1))
done


#!/bin/dash

if [ $# -ne 2 ] 2>/dev/null
then
  echo "Usage: ./echon.sh <number of lines> <string>"
  exit
fi

if ! [ "$1" -eq "$1" ] 2>/dev/null
then
  echo "./echon.sh: argument 1 must be a non-negative integer"
  exit
elif [ "$1" -lt 0 ] 2>/dev/null
then
  echo "./echon.sh: argument 1 must be a non-negative integer"
  exit
fi

count=$1
while [ "$count" -gt 0 ] 
do
  echo "$2"
  count=$((count - 1))
done



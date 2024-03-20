#!/bin/dash

small="Small files:"
medium="Medium-sized files:"
large="Large files:"

for x in *
do
  lines="$(wc -l < "$x")"

  if [ "$lines" -lt 10 ]
  then
    small="${small} $x"
  fi

  if [ "$lines" -lt 100 ] && [ "$lines" -ge 10 ]
  then
    medium="${medium} $x"
  fi
  
  if [ "$lines" -ge 100 ]
  then
    large="${large} $x"
  fi

done

echo "$small"
echo "$medium"
echo "$large"
#!/bin/dash

for file in *.jpg
do
  png=$(echo "$file" | sed -E s/\.jpg$/.png/)

  if test -f "$png"
  then
    echo "$png already exists"
  else
    convert "$file" "$png"
    rm "$file"
  fi

done

exit 0
#!/bin/dash

for arg in "$@"/*
do
  if test -d "$arg"
  then
    for file in "$arg"/*
    do
      title=$(echo "$file" | cut -d'-' -f2 | sed -E "s/ //")
      year=$(echo "$file" | cut -d'-' -f1 | cut -d'/' -f2 | cut -d',' -f2 | sed -E "s/ //g")
      artist=$(echo "$file" | cut -d'-' -f3 | cut -d'.' -f1 | sed -E "s/ //")
      album=$(echo "$file" | cut -d'/' -f2)
      track=$(echo "$file" | cut -d "-" -f1 | sed 's/.*\///' | sed 's/ $//g' )

      id3 -t "$title" "$file" > /dev/null
      id3 -y "$year" "$file" > /dev/null
      id3 -a "$artist" "$file" > /dev/null
      id3 -A "$album" "$file" > /dev/null
      id3 -T "$track" "$file" > /dev/null
    done
  else
    title=$(echo "$arg" | cut -d'-' -f2 | sed -E "s/ //")
    year=$(echo "$arg" | cut -d'-' -f1 | cut -d'/' -f2 | cut -d',' -f2 | sed -E "s/ //g")
    artist=$(echo "$arg" | cut -d'-' -f3 | cut -d'.' -f1 | sed -E "s/ //")
    album=$(echo "$arg" | cut -d'/' -f2)
    track=$(echo "$arg" | cut -d "-" -f1 | sed 's/.*\///' | sed 's/ $//g' )

    id3 -t "$title" "$arg" > /dev/null
    id3 -y "$year" "$arg" > /dev/null
    id3 -a "$artist" "$arg" > /dev/null
    id3 -A "$album" "$arg" > /dev/null
    id3 -T "$track" "$arg" > /dev/null
  fi
done

exit 0
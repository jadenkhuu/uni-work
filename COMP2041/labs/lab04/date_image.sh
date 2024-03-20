#!/bin/dash

date=$(ls -l "$1" | cut -d' ' -f6-8)
convert -gravity south -pointsize 36 -draw "text 0,10 '$date'" "$1" "$1"
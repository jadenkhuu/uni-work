#!/bin/dash

cut -d'|' -f3-5 | sort | uniq | cut -d' ' -f2 | sort | uniq -c | sort -nr | head -1 | sed 's/ *//g' | sed 's/ *[0-9]//g'



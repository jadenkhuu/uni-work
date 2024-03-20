#!/bin/dash

cut -d'|' -f2,3 | sort | uniq -c | grep -E ' *2 ' | sed 's/ *2 //g' | cut -d'|' -f1
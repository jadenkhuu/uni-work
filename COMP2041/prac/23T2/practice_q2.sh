#!/bin/dash

cut -d'|' -f3,5 | grep -E 'M$' | cut -d',' -f1 | sort | uniq
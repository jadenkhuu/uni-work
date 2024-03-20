#!/bin/dash

cut -d'|' -f3-5 | sort | uniq | cut -d' ' -f2 | sort
# sort -r -k4,4 | cut -d'|' -f3-5 | uniq | cut -d' ' -f2 | sort
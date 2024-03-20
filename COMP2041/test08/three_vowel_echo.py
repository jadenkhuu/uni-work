#!/usr/bin/env python3
import sys
import re

array = []

for word in sys.argv[1:]:
    if re.search(r'[aeiouAEIOU]{3}', word):        
        array.append(word)

print(" ".join(array))
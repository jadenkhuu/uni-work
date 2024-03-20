#!/usr/bin/env python3
import sys
import re

num = 0
word = sys.argv[1]

for line in sys.stdin:
    words = re.findall(rf'\b{word}\b', line, re.IGNORECASE)    
    num += len(words)
    
print(f"{word} occurred {num} times")


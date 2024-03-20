#!/usr/bin/env python3
import sys
import re

words = 0

for line in sys.stdin:
    words += len(re.findall(r'[A-Za-z]+', line))

print(f"{words} words")

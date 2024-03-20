#!/usr/bin/python3

import sys
import re

array = []

for line in sys.stdin:
    if re.search(r'M$', line):
        name = line.split(r'|')[2].split(r',')[0]
        array.append(name)

p = list(set(array))
p.sort()

print("\n".join(p))
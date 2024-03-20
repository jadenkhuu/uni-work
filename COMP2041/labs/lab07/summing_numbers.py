#!/usr/bin/env python3
import sys
import re

file = open(sys.argv[1], "r")

total = 0

for line in file:
  numbers = re.findall('\d+', line)
  for number in numbers:
    total = total + int(number)
    
print(total)
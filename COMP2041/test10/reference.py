#!/usr/bin/python3
import re

lines = []
while True:
    try:
        lines.append(input())        
    except EOFError:
        break

for line in lines:
    if re.search(r'^#[0-9]', line):
        num = line[1]
        print(lines[int(num) - 1])
    else:
        print(line)
#!/usr/bin/python3
import re

while True:
    try:
        line = input()
    except EOFError:
        break
    
    nums = re.findall(r'\d+\.\d+|\d+', line)
    for num in nums:
        roundNum = str(round(float(num)))
        line = line.replace(num, roundNum, 1)
        
    print(line)
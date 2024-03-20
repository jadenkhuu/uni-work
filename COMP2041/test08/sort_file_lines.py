#!/usr/bin/env python3
import sys

arr = []
f = open(sys.argv[1], "r")
lines = f.readlines()
f.close()

for line in lines:
    arr.append((len(line), line))
    
arr.sort(key=lambda x: (x[0], x[1]))
for i, j in arr:
    print(j, end="")
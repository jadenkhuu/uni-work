#!/usr/bin/env python3
import sys

file = open(sys.argv[1], "r")
lines = file.readlines()
reversed = lines[::-1]
file.close()

output = open(sys.argv[2], "w")

for line in reversed:
    output.write(line)
    
output.close()
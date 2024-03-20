#!/usr/bin/python3

import sys

file1 = open(sys.argv[1], "r")
file2 = open(sys.argv[2], "r")

lines1 = file1.readlines()
lines2 = file2.readlines()

reversed2 = list(reversed(lines2))
returnStr = ""

if lines1 == reversed2:
    returnStr += "Mirrored"
    print(returnStr)
    sys.exit(0)
else:
    returnStr += "Not mirrored: "

if len(lines1) != len(lines2):
    returnStr += f"different number of lines: {len(lines1)} versus {len(lines2)}"
else:
    i = 0
    while True:
        if reversed2[i] != lines1[i]:
            break
        i += 1
    returnStr += f"line {i+1} different"

print(returnStr)
#!/usr/bin/python3

import sys

file1 = open(sys.argv[2],  "r")
# chars = list("".join(file1.readlines()))
lines = file1.readlines()
lines = [ w[:-1] for w in lines ]
words = " ".join(lines).split()
file1.close()

num = int(sys.argv[1])

print(words)

returnStr = ""

for word in words:
    if len(word) <= num:
        
        continue
    elif 
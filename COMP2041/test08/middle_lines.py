#!/usr/bin/env python3
import sys

f = open(sys.argv[1], "r")
l = f.readlines()
f.close()

if len(l) == 0:
    exit()
elif len(l) % 2 == 0:
    mid = len(l) // 2
    print(l[mid - 1], end="")
    print(l[mid], end="")
else:
    mid = len(l) // 2
    print(l[mid], end="")
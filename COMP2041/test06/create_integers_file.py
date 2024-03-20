#!/usr/bin/env python3
import sys

file = open(sys.argv[3], "w")
for i in range(int(sys.argv[1]), int(sys.argv[2]) + 1):
    file.write(str(i))
    file.write("\n")
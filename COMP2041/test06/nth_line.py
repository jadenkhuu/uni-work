#!/usr/bin/env python3
import sys

file = open(sys.argv[2], "r")
lines = file.readlines()

if len(lines) < int(sys.argv[1]):
    exit()

print(f"{lines[int(sys.argv[1]) - 1].strip()}")

#!/usr/bin/env python3
import sys

n = sys.argv[1]
dictionary = {}

while True:
    try:
        arg = input()
        if arg in dictionary:
            dictionary[arg] += 1
            if dictionary[arg] == int(n):
                print(f"Snap: {arg}")
                sys.exit()
        else:
            dictionary[arg] = 1
    except EOFError:
        break
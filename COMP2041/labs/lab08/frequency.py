#!/usr/bin/env python3
import sys, re, glob

word = sys.argv[1]
freq = []

for file in glob.glob("lyrics/*.txt"):
    f = open(file, "r")
    name = re.sub(r'lyrics/', '', file)
    name = re.sub(r'.txt', '', name)
    name = re.sub(r'_', ' ', name)
    
    total = 0
    occurs = 0
    for line in f:
        total += len(re.findall(r'[A-Za-z]+', line))
        occurs += len(re.findall(rf'\b{word}\b', line, re.IGNORECASE))
    
    freq.append((f"{occurs:4}/{total:6} = {(occurs/total):.9f}", name))
    
freq.sort(key=lambda x: x[1])

for x in freq:
    print(f'{x[0]} {x[1]}')
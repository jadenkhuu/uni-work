#!/usr/bin/env python3
import sys, re, glob, math

data = []
for file in glob.glob("lyrics/*.txt"):
    name = re.sub(r'lyrics/', '', file)
    name = re.sub(r'.txt', '', name)
    name = re.sub(r'_', ' ', name)
    with open(file, 'r') as i:
        content = i.read()

    words = re.split(r'[^a-zA-Z]+', content)
    numWords = 0
    for word in words:
      if word:
          numWords += 1

    prob = 0
    for arg in sys.argv[1:]:
        word = arg.lower()
        count = 0
        for i in words:
          if word == i.lower():
            count += 1
        frequency = (count + 1) / numWords
        if frequency != 0:
            prob += math.log(frequency)

    data.append((name, prob))

data.sort(key=lambda x: x[0])

for name, prob in data:
    print(f"{prob:10.5f} {name}")
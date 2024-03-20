#!/usr/bin/env python3
import sys
import re

inputs = []
longest = 0
printLine = ""

while True:
  try:
    curr = input()
  except EOFError:
    break
  inputs.append(curr)

for line in inputs: 
  nums = re.findall(r'-?\d+.\d+|-?\d+', line)
  if nums:
    intNumbers = []
    for num in nums:
      splitNums = num.split()
      for sNums in splitNums:
        intNumbers.append(float(sNums))
      nums = intNumbers
      tempMax = max(nums)

    if tempMax > longest:
      longest = tempMax
      printLine = line
    elif tempMax == longest:
      printLine = printLine + "\n" + line

if printLine == "":
  exit
else:
  print(printLine)   
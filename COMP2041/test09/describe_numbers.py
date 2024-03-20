#!/usr/bin/env python3
import sys

numbers = list(sys.argv[1:])

numbers = [int(x) for x in numbers]

count = len(numbers)

unique = len(list(set(numbers)))

minimum = min(numbers)

maximum = max(numbers)

mean = sum(numbers)/count

median = sorted(numbers)[(count//2)]

mode = max(numbers, key=numbers.count)

sum = 0
for x in numbers:
    sum += x

product = 1 
for x in numbers:
    product *= x

print(f"count={count}")
print(f"unique={unique}")
print(f"minimum={minimum}")
print(f"maximum={maximum}")
if mean.is_integer():
  print(f"mean={int(mean)}")
else:
  print(f"mean={mean}")
print(f"median={median}")
print(f"mode={mode}")
print(f"sum={sum}")
print(f"product={product}")
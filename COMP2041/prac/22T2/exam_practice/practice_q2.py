#!/usr/bin/python3

# import sys
# import re

# count = 0
# for line in sys.stdin:
#     if '|3711/' in line:
#         count += 1

# print(count)

import sys
# import re
import csv

data = csv.reader(sys.stdin, delimiter='|')

count = 0
for course, stuId, name, program, gender in data:
    if '3711/' in program:
        count += 1

print(count)
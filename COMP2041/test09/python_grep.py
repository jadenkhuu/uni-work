#!/usr/bin/env python3

import sys
import re

file = open(sys.argv[2], 'r')
line = file.readlines()

for i in line:
    if re.search(f'{sys.argv[1]}', i):
        print(i, end="")
#!/usr/bin/python3

import re
import sys
import csv



line = "Hello123Jaden"
match = re.search(r"([A-Za-z]*)([0-9]*)([A-Za-z]*)", line)
print(match.group(2))



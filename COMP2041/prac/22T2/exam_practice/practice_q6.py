#!/usr/bin/python3

import sys
import csv
import re

data = csv.reader(sys.stdin, delimiter='|')
out = csv.writer(sys.stdout, delimiter='|')

for course, student, name, program, gender in data:
    name = re.fullmatch(r"(.*), (.*?)(\s*)", name)
    name = name.group(2) + " " + name.group(1) + name.group(3)
    out.writerow([course, student, name, program, gender])


###########################################################
# import sys
# import csv
# import re

# data = csv.reader(sys.stdin, delimiter='|')
# out = csv.writer(sys.stdout, delimiter='|')

# total = 0

# for course, stuid, name, progam, gender in csv.reader(sys.stdin, delimiter='|'):
#     name = re.fullmatch(r"(.*), (.*?)(\s*)", name)
#     name = name.group(2) + " " + name.group(1) + name.group(3)
#     out.writerow([course, stuid, name, progam, gender])
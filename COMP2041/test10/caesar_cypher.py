#!/usr/bin/python3

import re
import sys

array = []
shift = int(sys.argv[1])
shift = shift % 26
if shift < 0:
    shift += 26
    
while True:
    line = sys.stdin.read()
    if not line:
        break
    
    print_line = ""
    array.append(line)
    for i in line:
        if re.search(r'[a-z]', i):
            num = ord(i) + shift
            
            if (num > 122):
                num = num - 122 + 96

            print_line += chr(num)
            
        elif re.search(r'[A-Z]', i):
            num = ord(i) + shift

            if (num > 90):
                num = num - 90 + 64

            print_line += chr(num)
            
        else:
            print_line += i

    print(print_line, end="")
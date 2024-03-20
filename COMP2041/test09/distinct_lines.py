#!/usr/bin/python3 
import sys
import re

num = int(sys.argv[1])
lines = []

while True:    
    try:
        while len(list(set(lines))) < num:
            arg = input()
            
            new = arg.strip().lower()
            new = re.sub(r'\s+', ' ', new)
            
            lines.append(new)
            
        print(f"{num} distinct lines seen after {len(lines)} lines read.")
        break
    except EOFError:
        print(f"End of input reached after {len(lines)} lines read - {num} different lines not seen.")   
        break
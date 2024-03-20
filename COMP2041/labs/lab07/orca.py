#! /usr/bin/env python3
import sys
orcas = 0    
for i in range(1, len(sys.argv)):
    file = open(sys.argv[i], "r")
    for line in file.readlines():        
        if "Orca" in line:
            orcas = orcas + int(line.split()[1])      
    file.close()
print(f"{orcas} Orcas reported")
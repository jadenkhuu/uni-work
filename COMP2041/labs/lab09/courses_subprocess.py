#!/usr/bin/python3 

import sys
import subprocess

course = sys.argv[1]
url = f"https://timetable.unsw.edu.au/2023/{course}KENS.html"

p = subprocess.run(["curl", "--silent", url], text=True, capture_output=True)
p = subprocess.run(["grep", course], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['cut', '-d', '=', '-f', '3'], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['sed', '-E', 's/<.*>//g'], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['sed', '-E', 's/\"//g'], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['sed', '-E', 's/>/' '/g'], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['sed', '-E', 's/\.html/ /g'], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['grep', '-Ev', '^[A-z]{4}[0-9]{4} [A-z]{4}[0-9]{4}$'], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['sort'], input=p.stdout, text=True, capture_output=True)
p = subprocess.run(['uniq'], input=p.stdout, text=True, capture_output=True)

print(p.stdout, end="")
#!/usr/bin/python3 

import sys
import re
import urllib.request
from bs4 import BeautifulSoup

code = sys.argv[1]
website = f"https://timetable.unsw.edu.au/2023/{code}KENS.html"

IGNORE_WEBPAGE_ELEMENTS = set("[document] head meta style script title".split())

r = urllib.request.urlopen(website)
webpage = r.read().decode()
soup = BeautifulSoup(webpage, features="html5lib")

patt = rf'{code}\d{{4}}.html'
links = re.compile(patt)
courses = {}


for link in soup.find_all("a"):
  href = link.get("href")
  inner_text = link.text.strip()
  if href and links.match(href):
    href = href[:-5] 
    courses[href] = inner_text


for href, inner_text in sorted(courses.items()):
  print(f"{href} {inner_text}")
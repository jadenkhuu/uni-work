#!/bin/dash

course=$1
link="https://timetable.unsw.edu.au/2023/${course}KENS.html"

curl --location --silent $link | grep -E "$course" | cut -d'=' -f3 | sed -E s/'\.html'//g | sed -E s/\"//g | sed -E s/'<.*>'//g | sed -E s/'>'/' '/g | grep -Ev '[A-z]{4}[0-9]{4} [A-z]{4}[0-9]{4}' | sort | uniq

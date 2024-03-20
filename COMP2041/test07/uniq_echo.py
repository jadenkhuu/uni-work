#!/usr/bin/env python3
import sys

words = sys.argv
words.remove('uniq_echo.py')

unique_items = list(dict.fromkeys(words))

print(*unique_items)

sys.exit()
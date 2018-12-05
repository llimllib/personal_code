import re
import string

lower = string.ascii_lowercase
upper = string.ascii_uppercase
pat = "|".join(
    a + b for a, b in list(zip(lower, upper)) + list(zip(upper, lower)))
input = open("input.txt").read().strip()

min = 999999999
minchar = None
for char in lower:
    s = re.sub(f"{char}|{char.upper()}", "", input)
    ss = re.sub(pat, "", s)
    while s != ss:
        s = ss
        ss = re.sub(pat, "", s)
    if len(s) < min:
        min = len(s)
        minchar = char
print(f"{minchar}: {min}")

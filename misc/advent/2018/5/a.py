import re
import string

lower = string.ascii_lowercase
upper = string.ascii_uppercase
s = open("input.txt").read().strip()
pat = "|".join(
    a + b for a, b in list(zip(lower, upper)) + list(zip(upper, lower)))
ss = re.sub(pat, "", s)
while s != ss:
    s = ss
    ss = re.sub(pat, "", s)
print(len(s))

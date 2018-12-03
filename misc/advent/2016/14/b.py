import hashlib
import re
import sys


def md5(s):
    return hashlib.md5(s.encode()).hexdigest()


salt = 'abc'
i = -1
found = {}
key = 0
while 1:
    i += 1
    t = md5(f"{salt}{i}")
    match = re.search(r"(.)\1\1\1\1", t)
    if match:
        res = match.group(1)
        if res in found and found[res] + 1000 > i:
            print(f"got quint {res} {found[res]} {i} {t} {key}")
            key += 1
            if key == 65:
                print(i)
                sys.exit(0)
    match = re.search(r"(.)\1\1", t)
    if match:
        found[match.group(1)] = i

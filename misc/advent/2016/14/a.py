import hashlib
import re


def md5(s):
    return hashlib.md5(s.encode()).hexdigest()


salt = 'abc'
i = -1
triple = None
key = 0
while 1:
    i += 1
    if triple and triple + 999 < i:
        print(f"resetting triple, {triple}, {i}")
        triple = None
    t = md5(f"{salt}{i}")
    if not triple:
        if re.search(r"(.)\1\1", md5(f"{salt}{i}")):
            print(f"got triple {i} {salt}{i}")
            triple = i
            continue
    if triple and re.search(r"(.)\1\1\1\1", md5(f"{salt}{i}")):
        print(t)
        print(f"got quint {i}")
        key += 1
        if key == 64:
            print(i)
            break

import re

inp = open("input.txt").read().strip().split("\n")[2:]
nodes = []
for line in inp:
    a, b, size, used, avail, use = map(int, re.findall(r"\d+", line))
    nodes.append((a, b, size, used, avail, use))

n = 0
for a1, b1, size1, used1, avail1, _ in nodes:
    for a2, b2, size2, used2, avail2, _ in nodes:
        if a1 == a2 and b1 == b2:
            continue
        if used1 == 0:
            continue
        if used1 < avail2:
            n += 1
print(n)

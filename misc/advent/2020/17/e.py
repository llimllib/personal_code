from functools import reduce
from operator import or_

pts = set()
for y, line in enumerate(open("input.txt")):
    for x, char in enumerate(line):
        if char == "#":
            pts.add((x, y, 0, 0))


def add(v1, v2):
    return tuple(sum(a) for a in zip(v1, v2))


diffs = [
    (x, y, w, z)
    for x in (-1, 0, 1)
    for y in (-1, 0, 1)
    for w in (-1, 0, 1)
    for z in (-1, 0, 1)
    if (x, y, w, z) != (0, 0, 0, 0)
]


def neighbors(pt):
    return set(add(pt, diff) for diff in diffs)


def allneighbors(pts):
    return reduce(or_, (neighbors(p) for p in pts))


def countneighbors(pt):
    return sum(p in pts for p in neighbors(pt))


for i in range(6):
    updated = set()
    for pt in allneighbors(pts):
        n = countneighbors(pt)
        if pt in pts and n in (2, 3):
            updated.add(pt)
        elif n == 3:
            updated.add(pt)
    pts = updated
    print(i + 1, len(pts))

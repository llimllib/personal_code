pts = set()
for y, line in enumerate(open("input.txt")):
    for x, char in enumerate(line):
        if char == "#":
            pts.add((x, y, 0))


def add(v1, v2):
    return tuple(sum(a) for a in zip(v1, v2))


diffs = [
    (x, y, z)
    for x in (-1, 0, 1)
    for y in (-1, 0, 1)
    for z in (-1, 0, 1)
    if (x, y, z) != (0, 0, 0)
]


def allneighbors(pt):
    return (add(pt, diff) for diff in diffs)


def countneighbors(pt):
    return sum(p in pts for p in allneighbors(pt))


for i in range(6):
    updated = set()
    for pt in pts:
        if countneighbors(pt) in (2, 3):
            updated.add(pt)
    for pt in pts:
        for p in allneighbors(pt):
            if p not in pts:
                if countneighbors(p) == 3:
                    updated.add(p)
    pts = updated
    print(i + 1, len(pts))

import itertools
import sys


def parse(iter):
    pts = set()
    for row, line in enumerate(iter):
        for col, c in enumerate(line.strip()):
            if c != ".":
                pts.add((row, col))
    return pts


pts = sorted(parse(sys.stdin))
rows = {p[0] for p in pts}
cols = {p[1] for p in pts}

print(
    sum(
        abs(x1 - x2)
        + len(set(range(min(x1, x2) + 1, max(x1, x2))) - rows)
        + abs(y1 - y2)
        + len(set(range(min(y1, y2) + 1, max(y1, y2))) - cols)
        for (x1, y1), (x2, y2) in itertools.combinations(pts, 2)
    )
)
print(
    sum(
        abs(x1 - x2)
        + len(set(range(min(x1, x2) + 1, max(x1, x2))) - rows) * 999_999
        + abs(y1 - y2)
        + len(set(range(min(y1, y2) + 1, max(y1, y2))) - cols) * 999_999
        for (x1, y1), (x2, y2) in itertools.combinations(pts, 2)
    )
)

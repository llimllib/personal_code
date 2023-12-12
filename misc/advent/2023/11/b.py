import itertools
import sys

pts = [
    (row, col)
    for row, line in enumerate(sys.stdin)
    for col, c in enumerate(line.strip())
    if c != "."
]

print(
    sum(
        abs(x1 - x2)
        + len(set(range(min(x1, x2) + 1, max(x1, x2))) - {p[0] for p in pts})
        + abs(y1 - y2)
        + len(set(range(min(y1, y2) + 1, max(y1, y2))) - {p[1] for p in pts})
        for (x1, y1), (x2, y2) in itertools.combinations(pts, 2)
    )
)
print(
    sum(
        abs(x1 - x2)
        + len(set(range(min(x1, x2) + 1, max(x1, x2))) - {p[0] for p in pts}) * 999_999
        + abs(y1 - y2)
        + len(set(range(min(y1, y2) + 1, max(y1, y2))) - {p[1] for p in pts}) * 999_999
        for (x1, y1), (x2, y2) in itertools.combinations(pts, 2)
    )
)

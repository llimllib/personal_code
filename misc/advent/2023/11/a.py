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
ecols = set(range(max(cols))) - cols
erows = set(range(max(rows))) - rows

# 1. expand the universe
part1 = set()
part2 = set()
for row, col in pts:
    part1.add(
        (
            row + len([i for i in erows if i < row]),
            col + len([i for i in ecols if i < col]),
        )
    )
    part2.add(
        (
            row + len([i for i in erows if i < row]) * 999_999,
            col + len([i for i in ecols if i < col]) * 999_999,
        )
    )

# 2. sum the manhattan distance of each pair of points
print(
    sum(
        abs(x1 - x2) + abs(y1 - y2)
        for (x1, y1), (x2, y2) in itertools.combinations(part1, 2)
    )
)
print(
    sum(
        abs(x1 - x2) + abs(y1 - y2)
        for (x1, y1), (x2, y2) in itertools.combinations(part2, 2)
    )
)

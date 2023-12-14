from operator import itemgetter as get
import re
import sys

rocks, pins = [], []
maxrow = 0
for row, line in enumerate(sys.stdin):
    rocks += [[n.start(), row] for n in re.finditer("O", line)]
    pins += [[n.start(), row] for n in re.finditer("#", line)]
    maxrow = row + 1
maxcol = max(c for c, _ in rocks + pins)


def p(rocks, pins):
    for row in range(max(r for _, r in rocks + pins) + 1):
        for col in range(max(c for c, _ in rocks + pins) + 1):
            sys.stdout.write(
                "O" if [col, row] in rocks else "#" if [col, row] in pins else "."
            )
        sys.stdout.write("\n")


def cycle(rocks, pins):
    # north
    rocks.sort(key=get(1))
    for i, (col, row) in enumerate(sorted(rocks, key=get(1))):
        rocks[i][1] = (
            max([r for c, r in rocks + pins if r < row and c == col] + [-1]) + 1
        )
    # west
    rocks.sort(key=get(0))
    for i, (col, row) in enumerate(rocks):
        rocks[i][0] = (
            max([c for c, r in rocks + pins if c < col and r == row] + [-1]) + 1
        )
    # south
    rocks.sort(key=get(1), reverse=True)
    for i, (col, row) in enumerate(rocks):
        rocks[i][1] = (
            min([r for c, r in rocks + pins if r > row and c == col] + [maxrow]) - 1
        )
    # east
    rocks.sort(key=get(0), reverse=True)
    for i, (col, row) in enumerate(rocks):
        rocks[i][0] = (
            min([c for c, r in rocks + pins if c > col and r == row] + [maxcol + 1]) - 1
        )


weights = []
for i in range(1_000_000_000):
    cycle(rocks, pins)
    weight = sum(maxrow - r for _, r in rocks)
    print(i + 1, weight)

# 104394 is 134, 193, 252

# hypothesis: there's a cycle of 59 numbers startign at position 134
#
# 134 + 59 * ((1_000_000_000 // 59)-2) == 999999984
#
# so the number 16 past position 134 will be the answer
#
# number 150 is: 103445
#
# correct!

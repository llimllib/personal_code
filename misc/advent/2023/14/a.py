import re
import sys

rocks, pins = [], []
maxrow = 0
for row, line in enumerate(sys.stdin):
    rocks += [[n.start(), row] for n in re.finditer("O", line)]
    pins += [[n.start(), row] for n in re.finditer("#", line)]
    maxrow = row + 1

for i, (col, row) in enumerate(rocks):
    rocks[i][1] = max([r for c, r in rocks + pins if r < row and c == col] + [-1]) + 1

for row in range(max(r for _, r in rocks + pins) + 1):
    for col in range(max(c for c, _ in rocks + pins) + 1):
        sys.stdout.write(
            "O" if [col, row] in rocks else "#" if [col, row] in pins else "."
        )
    sys.stdout.write("\n")

print(sum(maxrow - r for _, r in rocks))

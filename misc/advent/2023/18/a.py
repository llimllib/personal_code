import sys


def pgrid(points):
    for row in range(
        int(min(p.real for p in points)), int(max(p.real for p in points)) + 1
    ):
        for col in range(
            int(min(p.imag for p in points)), int(max(p.imag for p in points)) + 1
        ):
            if complex(row, col) in points:
                sys.stdout.write("#")
            else:
                sys.stdout.write(".")
        sys.stdout.write("\n")


def floodfill(start, points: set[complex]):
    stack = [start]
    while stack:
        pt = stack.pop()
        for d in [1j, -1j, 1 + 0j, -1 + 0j]:
            if pt + d not in points:
                points.add(pt + d)
                stack.append(pt + d)


moves = []
for line in sys.stdin:
    dir, dist, color = line.strip().split(" ")
    moves.append((dir, int(dist), color.strip("()")))

dirs = {"R": 1j, "L": -1j, "U": -1 + 0j, "D": 1 + 0j}

points = [0j]
depth = 0
for dir, dist, _ in moves:
    cur = points[-1]
    depth += dist
    for i in range(1, dist + 1):
        points.append(cur + dirs[dir] * i)


points = set(points)
pgrid(points)
l1 = len(points)
floodfill(1 + 1j, points)
l2 = len(points)
print("part 1:", depth + (l2 - l1))

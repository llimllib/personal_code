import sys
import ipdb
from math import *

f = sys.argv[1] if len(sys.argv) > 1 else "input.txt"
lines = [l for l in open(f)]

x = 0
y = 0
dx = 1
dy = 0

rotations = {
    90: {(0, 1): (1, 0), (1, 0): (0, -1), (0, -1): (-1, 0), (-1, 0): (0, 1),},
    180: {(0, 1): (0, -1), (1, 0): (-1, 0), (0, -1): (0, 1), (-1, 0): (1, 0),},
    270: {(0, 1): (-1, 0), (1, 0): (0, 1), (0, -1): (1, 0), (-1, 0): (0, -1),},
}

for line in lines:
    cmd, n = line[0], int(line[1:])
    print(x, y, dx, dy, cmd, n)
    if cmd == "F":
        x += dx * n
        y += dy * n
    elif cmd == "N":
        y += n
    elif cmd == "E":
        x += n
    elif cmd == "W":
        x -= n
    elif cmd == "S":
        y -= n
    elif cmd == "R":
        (dx, dy) = rotations[n][(dx, dy)]
    elif cmd == "L":
        (dx, dy) = rotations[n][(dx, dy)]
        if n != 180:
            dx = -dx
            dy = -dy
    else:
        raise Exception(f"failed to handle command {cmd}, {n}")


print(x, y, abs(x) + abs(y))

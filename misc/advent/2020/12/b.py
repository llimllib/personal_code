import sys
import ipdb
from math import *

f = sys.argv[1] if len(sys.argv) > 1 else "input.txt"
lines = [l for l in open(f)]

x = 0
y = 0
dx = 1
dy = 0
# The waypoint starts 10 units east and 1 unit north relative to the ship.
wx = 10
wy = 1

rotations = {
    90: {(0, 1): (1, 0), (1, 0): (0, -1), (0, -1): (-1, 0), (-1, 0): (0, 1),},
    180: {(0, 1): (0, -1), (1, 0): (-1, 0), (0, -1): (0, 1), (-1, 0): (1, 0),},
    270: {(0, 1): (-1, 0), (1, 0): (0, 1), (0, -1): (1, 0), (-1, 0): (0, -1),},
}

for line in lines:
    cmd, n = line[0], int(line[1:])
    print(f"xy: {x}, {y}, wxy: {wx}, {wy} cmd: {cmd}, {n}")
    if cmd == "F":
        x += wx * n
        y += wy * n
    elif cmd == "N":
        wy += n
    elif cmd == "E":
        wx += n
    elif cmd == "W":
        wx -= n
    elif cmd == "S":
        wy -= n
    elif cmd == "R":
        if n == 90:
            wx, wy = wy, -wx
        if n == 180:
            wx, wy = -wx, -wy
        if n == 270:
            wx, wy = -wy, wx
    elif cmd == "L":
        if n == 90:
            wx, wy = -wy, wx
        if n == 180:
            wx, wy = -wx, -wy
        if n == 270:
            wx, wy = wy, -wx
    else:
        raise Exception(f"failed to handle command {cmd}, {n}")


print(x, y, abs(x) + abs(y))

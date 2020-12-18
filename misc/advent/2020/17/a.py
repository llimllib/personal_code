import numpy as np

ACTIVE = "#"
INACTIVE = "."

example = """.#.
..#
###
"""

# example = open("input.txt").read()

lines = example.split("\n")

steps = 6

w = 50
zw = (steps * 2) + 2
cube = np.full((w, w, zw), ".")

z = zw // 2
y = (w // 2) - (len(lines) // 2)
for line in lines:
    x = (w // 2) - (len(line) // 2)
    for c in line:
        cube[y][x][z] = c
        x += 1
    y += 1

coords = [
    (x, y, z)
    for x in (-1, 0, 1)
    for y in (-1, 0, 1)
    for z in (-1, 0, 1)
    if (x, y, z) != (0, 0, 0)
]
changes = []
for i in range(steps):
    for y in range(w):
        for x in range(w):
            for z in range(zw):
                active_neighbors = 0
                for dy, dx, dz in coords:
                    if 0 <= y + dy < w and 0 <= x + dx < w and 0 <= z + dz < zw:
                        if cube[y + dy][x + dx][z + dz] == ACTIVE:
                            active_neighbors += 1
                if cube[y][x][z] == ACTIVE:
                    if active_neighbors not in (2, 3):
                        changes.append((x, y, z, INACTIVE))
                else:
                    if active_neighbors == 3:
                        changes.append((x, y, z, ACTIVE))
    for x, y, z, state in changes:
        cube[y][x][z] = state
    print(i + 1, np.count_nonzero(cube == "#"))

print(np.count_nonzero(cube == "#"))

from itertools import chain
import re
import sys

directions = []

# for line in open("small.txt"):
for line in open("input.txt"):
    directions.append(re.findall(r"ne|se|nw|sw|e|w", line))

# https://www.redblobgames.com/grids/hexagons/#coordinates-cube
blacktiles = set()
for line in directions:
    x = 0
    y = 0
    z = 0
    for d in line:
        if d == "e":
            x += 1
            y -= 1
        elif d == "w":
            x -= 1
            y += 1
        elif d == "se":
            z += 1
            y -= 1
        elif d == "sw":
            x -= 1
            z += 1
        elif d == "ne":
            x += 1
            z -= 1
        elif d == "nw":
            z -= 1
            y += 1
    if (x, y, z) in blacktiles:
        blacktiles.remove((x, y, z))
    else:
        blacktiles.add((x, y, z))

print("part 1:", len(blacktiles))


def flatten(list_of_lists):
    "Flatten one level of nesting"
    return chain.from_iterable(list_of_lists)


# https://www.redblobgames.com/grids/hexagons/#coordinates-cube
diffs = [(0, 1, -1), (1, 0, -1), (1, -1, 0), (0, -1, 1), (-1, 0, 1), (-1, 1, 0)]


def allneighbors(tiles):
    return set(
        (x + dx, y + dy, z + dz) for (x, y, z) in tiles for (dx, dy, dz) in diffs
    )


def neighbors(tile, tiles):
    n = 0
    for dx, dy, dz in diffs:
        if (tile[0] + dx, tile[1] + dy, tile[2] + dz) in tiles:
            n += 1
    return n


for i in range(100):
    sys.stdout.write(".")
    updated = set()
    for tile in allneighbors(blacktiles):
        n = neighbors(tile, blacktiles)
        if tile in blacktiles and n in (1, 2):
            updated.add(tile)
        if tile not in blacktiles and n == 2:
            updated.add(tile)
    blacktiles = updated
sys.stdout.write("\n")

print("part 2:", len(blacktiles))

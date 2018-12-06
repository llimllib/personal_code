from collections import Counter
from itertools import chain


def dist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def removeall(grid, item):
    if item in ['.', '_', '#']: return grid
    return [[x if x != item else '#' for x in row] for row in grid]


def flatten(listOfLists):
    "Flatten one level of nesting"
    return chain.from_iterable(listOfLists)


def do(infile):
    coords = set(tuple(map(int, line.split(","))) for line in infile)
    names = {c: i for i, c in enumerate(coords)}

    maxx = max(coords)[0] + 1
    maxy = max(coords, key=lambda x: x[1])[1] + 1
    print(f"{maxx} {maxy}")

    grid = [['_'] * maxx for _ in range(maxy)]

    for x, y in coords:
        grid[y][x] = names[(x, y)]

    # print("\n".join("".join(map(str, row)) for row in grid))

    # let's start with dumb brute force
    for x in range(maxx):
        for y in range(maxy):
            pt = (x, y)
            if pt in coords:
                continue
            dists = [(dist(pt, c), c) for c in coords]
            mind = min(dists)[0]
            if len([d for d in dists if d[0] == mind]) > 1:
                grid[y][x] = '.'
            else:
                grid[y][x] = names[min(dists)[1]]

    # print('🔴🔴🔴🔴🔴🔴🔴🔴🔴')
    # print("\n".join("".join(map(str, row)) for row in grid))

    # now eliminate all letters on an edge:
    for char in set(grid[0]):
        grid = removeall(grid, char)
    for char in set(grid[maxy - 1]):
        grid = removeall(grid, char)
    for char in set(row[0] for row in grid):
        grid = removeall(grid, char)
    for char in set(row[maxx - 1] for row in grid):
        grid = removeall(grid, char)

    # print('🔴🔴🔴🔴🔴🔴🔴🔴🔴')
    # print("\n".join("".join(map(str, row)) for row in grid))

    c = Counter(flatten(grid))
    del c['#']
    print(f"{c.most_common(1)}")


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))

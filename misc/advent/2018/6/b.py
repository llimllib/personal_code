from collections import Counter
from itertools import chain


def dist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def flatten(listOfLists):
    "Flatten one level of nesting"
    return chain.from_iterable(listOfLists)


def do(infile, size):
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
            dists = [dist(pt, c) for c in coords]
            if sum(dists) < size:
                grid[y][x] = '#'
            else:
                grid[y][x] = '.'

    # print('🔴🔴🔴🔴🔴🔴🔴🔴🔴')
    # print("\n".join("".join(map(str, row)) for row in grid))

    c = Counter(flatten(grid))
    print(f"{c['#']}")


if __name__ == "__main__":
    do(open("small.txt"), 32)
    do(open("input.txt"), 10000)

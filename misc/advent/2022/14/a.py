from collections import defaultdict
import itertools
import sys


def flatten(list_of_lists):
    return itertools.chain.from_iterable(list_of_lists)


def parse(text):
    lines = []
    for line in text.strip().split("\n"):
        lines.append([tuple(map(int, y.split(","))) for y in line.split("->")])
    return lines


def gprint(grid, extent, grain=None):
    for row in range(extent[0][1], extent[1][1] + 1):
        for col in range(extent[0][0], extent[1][0] + 1):
            if row in grid[col]:
                sys.stdout.write(grid[col][row])
            elif grain and grain == (col, row):
                sys.stdout.write("o")
            else:
                sys.stdout.write(".")
        print()


def makegrid(text):
    lines = parse(text)

    maxy = max(flatten(lines), key=lambda x: x[1])[1]

    columns = defaultdict(dict)

    for line in lines:
        start = line[0]
        for pt in line[1:]:
            mx = min(pt[0], start[0])
            for col in range(mx, mx + abs(pt[0] - start[0]) + 1):
                columns[col][pt[1]] = "#"

            my = min(pt[1], start[1])
            for row in range(my, my + abs(pt[1] - start[1]) + 1):
                columns[pt[0]][row] = "#"

            start = pt
    return columns, maxy


def run(grid, limit):
    row = 0
    col = 500

    grains = 0
    while row <= limit:
        if row + 1 not in grid[col]:
            row += 1
            continue
        elif row + 1 not in grid[col - 1]:
            row += 1
            col -= 1
            continue
        elif row + 1 not in grid[col + 1]:
            row += 1
            col += 1
            continue

        grid[col][row] = "*"
        row = 0
        col = 500
        grains += 1
        # gprint(grid, ((485, 0), (503, 9)), (col, row))

    return grains


def run2(grid, limit):
    row = 0
    col = 500

    grains = 0
    while True:
        if row == limit + 1:
            grid[col][row] = "*"
            row = 0
            col = 500
            grains += 1
            continue
        elif row + 1 not in grid[col]:
            row += 1
            continue
        elif row + 1 not in grid[col - 1]:
            row += 1
            col -= 1
            continue
        elif row + 1 not in grid[col + 1]:
            row += 1
            col += 1
            continue
        elif row == 0 and col == 500:
            grid[col][row] = "*"
            grains += 1
            break

        grid[col][row] = "*"
        row = 0
        col = 500
        grains += 1

    return grains


grid, limit = makegrid(
    """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""
)
grains = run(grid, limit)
print(grains)

grid, limit = makegrid(open("input.txt").read())
grains = run(grid, limit)
print(grains)

grid, limit = makegrid(
    """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""
)
grains = run2(grid, limit)
print(grains)

grid, limit = makegrid(open("input.txt").read())
grains = run2(grid, limit)
print(grains)

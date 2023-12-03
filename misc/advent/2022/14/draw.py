from collections import defaultdict
import itertools
import sys

from PIL import Image, ImageDraw, ImageFilter


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


def imprint(grid, step, grain=None):
    w, h = (600, 600)
    shiftx = 200
    black = (0, 0, 0, 255)
    yellow = (245, 245, 123, 255)
    white = (255, 255, 255, 255)
    with Image.new("RGBA", (w, h), white) as im:
        draw = ImageDraw.Draw(im)
        for row in range(w):
            for col in range(shiftx, shiftx + h):
                if row in grid[col] and grid[col][row] == "#":
                    draw.point((col - shiftx, row), fill=black)
                if row in grid[col] and grid[col][row] == "*":
                    draw.point((col - shiftx, row), fill=yellow)
                if grain and grain == (col, row):
                    draw.point((col - shiftx, row), fill=yellow)
        im.save(f"im/{step:05}.png")


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
    step = 0
    while True:
        step += 1
        imprint(grid, step, (col, row))
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

        grid[col][row] = "*"
        grains += 1
        if row == 0 and col == 500:
            break

        row = 0
        col = 500

    return grains


grid, limit = makegrid(open("input.txt").read())
grains = run2(grid, limit)
print("part 2:", grains)

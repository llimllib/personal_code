import re
import sys

from IPython.core import ultratb

sys.excepthook = ultratb.FormattedTB(mode="Verbose", color_scheme="Linux", call_pdb=1)

# from ipdb import set_trace as t


def pmap(map_, loc):
    for row in range(len(map_)):
        for col in range(len(map_[row])):
            if (row, col) == loc:
                sys.stdout.write("@")
            else:
                sys.stdout.write(map_[row][col])
        sys.stdout.write("\n")


def parse(text):
    lines = text.rstrip().split("\n")
    directions = re.split("([RL])", lines.pop())
    lines.pop()

    maxlen = max(len(l) for l in lines)

    map = [list(row) + [" " for _ in range(maxlen - len(row))] for row in lines]
    return (map, (0, map[0].index(".")), directions)


rotations = {
    "RR": "D",
    "RL": "U",
    "DR": "L",
    "DL": "R",
    "LR": "U",
    "LL": "D",
    "UR": "R",
    "UL": "L",
}


def move(map_, loc, direction, steps):
    row, col = loc
    width = len(map_[row])
    height = len(map_)
    while steps > 0:
        if direction == "R":
            while map_[row][(col + 1) % width] == " ":
                col = (col + 1) % width
            if map_[row][(col + 1) % width] == "#":
                return loc
            loc = (row, (col + 1) % width)
            row, col = loc
            steps -= 1
        elif direction == "L":
            while map_[row][(col - 1) % width] == " ":
                col = (col - 1) % width
            if map_[row][(col - 1) % width] == "#":
                return loc
            loc = (row, (col - 1) % width)
            row, col = loc
            steps -= 1
        elif direction == "D":
            while map_[(row + 1) % height][col] == " ":
                row = (row + 1) % height
            if map_[(row + 1) % height][col] == "#":
                return loc
            loc = ((row + 1) % height, col)
            row, col = loc
            steps -= 1
        elif direction == "U":
            while map_[(row - 1) % height][col] == " ":
                row = (row - 1) % height
            if map_[(row - 1) % height][col] == "#":
                return loc
            loc = ((row - 1) % height, col)
            row, col = loc
            steps -= 1
        else:
            raise AssertionError("TODO")
    return loc


DIRSCORE = {
    "R": 0,
    "D": 1,
    "L": 2,
    "U": 3,
}


def run(map_, loc, directions, verbose=False):
    direction = "R"
    for step in directions:
        if step in ("R", "L"):
            direction = rotations[direction + step]
            continue

        loc = move(map_, loc, direction, int(step))
        if verbose:
            pmap(map_, loc)
    return 1000 * (loc[0] + 1) + 4 * (loc[1] + 1) + DIRSCORE[direction]


sample = """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""
map_, loc, directions = parse(sample)
assert run(map_, loc, directions, verbose=True) == 6032


map_, loc, directions = parse(open("input.txt").read())
print("part 1:", run(map_, loc, directions, verbose=False))

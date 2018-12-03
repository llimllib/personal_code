from collections import defaultdict
import sys

NORTH = 0
EAST = 1
SOUTH = 2
WEST = 3

CLEAN = 0
INFECTED = 1


def left(dir):
    if dir == NORTH: return WEST
    if dir == WEST: return SOUTH
    if dir == SOUTH: return EAST
    if dir == EAST: return NORTH


def right(dir):
    if dir == NORTH: return EAST
    if dir == EAST: return SOUTH
    if dir == SOUTH: return WEST
    if dir == WEST: return NORTH


def move(pos, dir):
    x, y = pos
    if dir == NORTH: return (x, y - 1)
    if dir == EAST: return (x + 1, y)
    if dir == SOUTH: return (x, y + 1)
    if dir == WEST: return (x - 1, y)


def print_map(map_):
    minx = min(map_)[0]
    maxx = max(map_)[0]
    miny = min(x[1] for x in map_)
    maxy = max(x[1] for x in map_)
    for y in range(miny, maxy + 1):
        for x in range(minx, maxx + 1):
            sys.stdout.write(f"{'#' if map_[(x, y)] else '.'} ")
        sys.stdout.write("\n")
    sys.stdout.write("------------\n")


def go(grid, iters=10000):
    center = len(grid) // 2
    map_ = defaultdict(int)
    for y in range(len(grid)):
        for x in range(len(grid)):
            map_[(x - center, y - center)] = grid[y][x]

    pos = (0, 0)
    dir = NORTH
    infections = 0
    # print_map(map_)
    for i in range(iters):
        if map_[pos] == INFECTED:
            map_[pos] = CLEAN
            dir = right(dir)
            pos = move(pos, dir)
        else:
            map_[pos] = INFECTED
            dir = left(dir)
            pos = move(pos, dir)
            infections += 1
        # print_map(map_)

    print(f"infections: {infections}")


if __name__ == "__main__":
    grid = list(
        list(map(int, list(x.replace('.', '0').replace('#', '1').strip())))
        for x in open('sample.txt'))
    #go(grid, 70)
    #go(grid)
    grid = list(
        list(map(int, list(x.replace('.', '0').replace('#', '1').strip())))
        for x in open('input.txt'))
    go(grid)

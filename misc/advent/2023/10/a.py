from enum import Enum
import sys

YELLOW_BG = "\033[43m"
GREEN_BG = "\033[42m"
RESET = "\033[0m"
CLEARSC = "\033[2J"
GOHOME = "\033[0;0H"


def pgrid(grid, loop=None):
    if loop:
        loop = set(loop)
    for row, line in enumerate(grid):
        for col, c in enumerate(line):
            if loop and (col, row) in loop:
                sys.stdout.write(f"{GREEN_BG}{c}{RESET}")
            else:
                sys.stdout.write(c)
        print()


def parse(iter):
    grid = []
    table = "".maketrans("|-LJ7F.S", "│─└┘┐┌░╳")
    start = (0, 0)
    for row, line in enumerate(iter):
        col = line.find("S")
        if col != -1:
            start = (col, row)
        grid.append(list(line.strip().translate(table)))
    return (grid, start)


class Direction(Enum):
    ANY = 1
    LEFT = 2
    RIGHT = 3
    DOWN = 4
    UP = 5


transitions = {
    (Direction.LEFT, "─"): Direction.LEFT,
    (Direction.LEFT, "└"): Direction.UP,
    (Direction.LEFT, "┌"): Direction.DOWN,
    (Direction.RIGHT, "─"): Direction.RIGHT,
    (Direction.RIGHT, "┘"): Direction.UP,
    (Direction.RIGHT, "┐"): Direction.DOWN,
    (Direction.UP, "│"): Direction.UP,
    (Direction.UP, "┐"): Direction.LEFT,
    (Direction.UP, "┌"): Direction.RIGHT,
    (Direction.DOWN, "│"): Direction.DOWN,
    (Direction.DOWN, "┘"): Direction.LEFT,
    (Direction.DOWN, "└"): Direction.RIGHT,
}


def firstmove(grid, cell):
    col, row = cell
    moves = []
    if col > 0 and (Direction.LEFT, grid[row][col - 1]) in transitions:
        moves.append(
            (transitions[(Direction.LEFT, grid[row][col - 1])], [(col - 1, row)])
        )
    if col < len(grid[0]) and (Direction.RIGHT, grid[row][col + 1]) in transitions:
        moves.append(
            (transitions[(Direction.RIGHT, grid[row][col + 1])], [(col + 1, row)])
        )
    if row > 0 and (Direction.UP, grid[row - 1][col]) in transitions:
        moves.append(
            (transitions[(Direction.UP, grid[row - 1][col])], [(col, row - 1)])
        )
    if row < len(grid) and (Direction.DOWN, grid[row + 1][col]) in transitions:
        moves.append(
            (transitions[(Direction.DOWN, grid[row + 1][col])], [(col, row + 1)])
        )

    return moves


def nextmove(grid, cell, direction):
    col, row = cell
    if direction == Direction.LEFT and col > 0:
        return (transitions.get((direction, grid[row][col - 1]), None), (col - 1, row))
    if direction == Direction.RIGHT and col < len(grid[0]):
        return (transitions.get((direction, grid[row][col + 1]), None), (col + 1, row))
    if direction == Direction.UP and row > 0:
        return (transitions.get((direction, grid[row - 1][col]), None), (col, row - 1))
    if direction == Direction.DOWN and row < len(grid):
        return (transitions.get((direction, grid[row + 1][col]), None), (col, row + 1))


def findloop(grid, start):
    paths = firstmove(grid, start)
    while paths:
        dir, path = paths.pop()
        dir, cell = nextmove(grid, path[-1], dir)
        if cell == start:
            return path + [cell]
        if dir:
            paths.append((dir, path + [cell]))

    raise Exception("no loop found")


grid, start = parse(sys.stdin)
loop = findloop(grid, start)
print(f"{CLEARSC}{GOHOME}")
print("max length: ", len(loop) // 2)
pgrid(grid, loop)

# adding a heuristic improves speed from 12s to .2s
import sys
from typing import Tuple, Generator


def parse(it):
    grid = []
    for line in it:
        if not line.strip():
            continue
        grid.append(list(map(int, list(line.strip()))))
    return grid


def mod(n):
    if n > 9:
        return n % 9
    return n


def expand(grid):
    h = len(grid) * 5
    w = len(grid[0]) * 5
    hi = len(grid)
    wi = len(grid[0])
    bigg = [[0] * w for _ in range(h)]
    for row in range(len(grid) * 5):
        for col in range(len(grid[0]) * 5):
            bigg[row][col] = mod(grid[row % hi][col % wi] + (row // hi) + (col // wi))
    return bigg


def neighbors(
    xy: Tuple[int, int], h: int, w: int
) -> Generator[Tuple[int, int], None, None]:
    x, y = xy
    if x > 0:
        yield (x - 1, y)
    if x < w - 1:
        yield (x + 1, y)
    if y > 0:
        yield (x, y - 1)
    if y < h - 1:
        yield (x, y + 1)


def showpath(grid, path):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if (row, col) in path:
                sys.stdout.write(f"\u001b[31m{grid[row][col]}\u001b[0m")
            else:
                sys.stdout.write(f"{grid[row][col]}")
        sys.stdout.write("\n")


def path(loc, steps):
    path = [loc]
    step = steps[loc]
    while step:
        path.append(step)
        step = steps[step]
    return path


def heur(goal, node):
    # this heuristic is inadmissible, but it works on my example and makes it
    # much faster. Got a report from somebody else that it doesn't work on
    # theirs, so probably only works on mine by chance. Would be interesting to
    # think about a heuristic that is admissable and also much better
    #
    # An admissable heuristic is one that's non-negative, and never greater
    # than the actual cost of the path from node to goal
    # return abs(goal[0] - node[0]) + abs(goal[1] - node[1]) * 10
    return abs(goal[0] - node[0]) + abs(goal[1] - node[1])


def search(grid, goal):
    h = len(grid)
    w = len(grid[0])
    start = (0, 0)
    frontier = [(0, start)]
    steps = {start: None}
    costs = {start: 0}
    visited = 0
    while frontier:
        visited += 1
        _, (row, col) = frontier.pop()

        if (row, col) == goal:
            break

        for neighbor in neighbors((row, col), h, w):
            cost = costs[(row, col)] + grid[row][col]
            if neighbor not in costs or cost < costs[neighbor]:
                costs[neighbor] = cost
                frontier.append((cost + heur(goal, neighbor), neighbor))
                # poor man's priority queue
                frontier.sort(reverse=True)
                steps[neighbor] = (row, col)
    print(f"Visited {visited} nodes")
    return steps


grid = parse(open("small.txt"))
goal = (len(grid) - 1, len(grid[0]) - 1)
steps = search(grid, goal)
# showpath(grid, path(goal, steps))
print(sum(grid[row][col] for (row, col) in path(goal, steps)) - grid[0][0])

print("-----------")

grid = parse(open("input.txt"))
goal = (len(grid) - 1, len(grid[0]) - 1)
steps = search(grid, goal)
# showpath(grid, path(goal, steps))
print(sum(grid[row][col] for (row, col) in path(goal, steps)) - grid[0][0])

print("-----------")

grid = expand(parse(open("small.txt")))
goal = (len(grid) - 1, len(grid[0]) - 1)
steps = search(grid, goal)
# showpath(grid, path(goal, steps))
print(sum(grid[row][col] for (row, col) in path(goal, steps)) - grid[0][0])

print("-----------")

grid = expand(parse(open("input.txt")))
goal = (len(grid) - 1, len(grid[0]) - 1)
steps = search(grid, goal)
# showpath(grid, path(goal, steps))
print(sum(grid[row][col] for (row, col) in path(goal, steps)) - grid[0][0])

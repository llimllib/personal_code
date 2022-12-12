import heapq
import sys


def parse(txt):
    map = []
    for row, line in enumerate(txt.strip().split("\n")):
        r = []
        map.append(r)
        for col, c in enumerate(line.strip()):
            if c not in ["S", "E"]:
                r.append(ord(c) - ord("a"))
            elif c == "S":
                start = (row, col)
                r.append(0)
            else:
                goal = (row, col)
                r.append(25)
    return map, start, goal


def neighbors(pos, height, width):
    row, col = pos
    if row > 0:
        yield (pos[0] - 1, pos[1])
    if row < height - 1:
        yield (pos[0] + 1, pos[1])
    if col > 0:
        yield (pos[0], pos[1] - 1)
    if col < width - 1:
        yield (pos[0], pos[1] + 1)


def moves(map_, pos):
    v = map_[pos[0]][pos[1]]
    for row, col in neighbors(pos, len(map_), len(map_[0])):
        if map_[row][col] <= v + 1:
            yield (row, col)


def showpath(map_, path):
    for row in range(len(map_)):
        for col in range(len(map_[0])):
            if (row, col) in path:
                sys.stdout.write(f"\u001b[31m{chr(ord('a') + map_[row][col])}\u001b[0m")
            else:
                sys.stdout.write(f"{chr(ord('a') + map_[row][col])}")
        sys.stdout.write("\n")


def find(map_, start, goal):
    frontier = [(0, start)]
    steps = {start: None}
    costs = {start: 0}
    while frontier:
        _, loc = heapq.heappop(frontier)

        if loc == goal:
            break

        for move in moves(map_, loc):
            cost = costs[loc] + 1
            if move not in costs or cost < costs[move]:
                costs[move] = cost
                heapq.heappush(frontier, (cost, move))
                steps[move] = loc
    return steps


def findpath(steps, pos):
    path = [pos]
    step = steps[pos]
    while step:
        path.append(step)
        step = steps[step]
    return path


def find_shortest_start(map_, start, goal):
    min_ = 10000
    shortestpath = None
    for r in range(len(map_)):
        for c in range(len(map_[0])):
            if map_[r][c] == 0:
                try:
                    path = findpath(find(map_, (r, c), goal), goal)
                except KeyError:
                    continue
                if len(path) < min_:
                    min_ = len(path)
                    shortestpath = path
    return min_, shortestpath


map_, start, goal = parse(
    """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""
)
steps = find(map_, start, goal)
path = findpath(steps, goal)
print(len(path) - 1)
showpath(map_, path)

print("\n---------\n")

map_, start, goal = parse(open("input.txt").read().strip())
steps = find(map_, start, goal)
path = findpath(steps, goal)
showpath(map_, path)
print(len(path) - 1)

print("\n---------\n")

map_, start, goal = parse(
    """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""
)
l, path = find_shortest_start(map_, start, goal)
showpath(map_, path)
print(l - 1)

print("\n---------\n")

map_, start, goal = parse(open("input.txt").read().strip())
l, path = find_shortest_start(map_, start, goal)
showpath(map_, path)
print(l - 1)

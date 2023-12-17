import heapq
from collections import defaultdict, deque
import sys

Node = tuple[int, int]

GREEN_BG = "\x1b[42m"
RESET = "\x1b[0m"

# fmt: off
def nabes(node: Node) -> list[Node]:
    row, col = node
    nodes = []
    if row > 0:   nodes.append((row-1, col))
    if row < h-1: nodes.append((row+1, col))
    if col > 0:   nodes.append((row, col-1))
    if col < w-1: nodes.append((row, col+1))
    return nodes
# fmt: on


def pgrid(grid: list[list[int]], path: list[Node]):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            sys.stdout.write(
                str(grid[row][col])
                if (row, col) not in path
                else f"{GREEN_BG}{grid[row][col]}{RESET}"
            )
        sys.stdout.write("\n")


grid = [[int(x) for x in line.strip()] for line in sys.stdin]
w = len(grid[0])
h = len(grid)

start = (0, 0)
goal = (h - 1, w - 1)
frontier: list[int, Node] = [(0, (0, 0))]
heapq.heapify(frontier)
paths = {}
costs = {start: 0}


while len(frontier):
    _, cur = heapq.heappop(frontier)
    if cur == goal:
        break
    for next in nabes(cur):
        row, col = next
        cost = costs[cur] + grid[row][col]
        if next not in costs or cost < costs[next]:
            costs[next] = cost
            heapq.heappush(frontier, (cost, next))
            paths[next] = cur

node = goal
path = []
while node != start:
    path.append(node)
    node = paths[node]
pgrid(grid, path)

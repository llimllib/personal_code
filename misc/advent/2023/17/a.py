import heapq
from collections import defaultdict, deque
import sys
import ipdb

Node = tuple[int, int]

x_BG = "\x1b[43m"
GREEN_BG = "\x1b[42m"
RESET = "\x1b[0m"


def pgrid(grid: list[list[int]], path: list[Node], cur: Node = None):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if (row, col) == cur:
                sys.stdout.write(f"{x_BG}{grid[row][col]}{RESET}")
            elif (row, col) in path:
                sys.stdout.write(f"{GREEN_BG}{grid[row][col]}{RESET}")
            else:
                sys.stdout.write(str(grid[row][col]))
        sys.stdout.write("\n")


def valid(paths, current: Node, node_test: Node) -> bool:
    a, b, c = (
        paths.get(current),
        paths.get(paths.get(current)),
        paths.get(paths.get(paths.get(current))),
    )
    if not a or not b or not c:
        return True
    # can't go four steps in the same direction
    if (
        node_test[0] == current[0] == a[0] == b[0] == c[0]
        or node_test[1] == current[1] == a[1] == b[1] == c[1]
    ):
        # print(current, a, b, "❌", node_test)
        return False
    # can't turn around 180*
    if node_test == a:
        return False
    # print(current, a, b, "✓", node_test)
    return True


# fmt: off
def nabes(paths, node: Node) -> list[Node]:
    row, col = node
    nodes = []
    if row > 0:   nodes.append((row-1, col))
    if row < h-1: nodes.append((row+1, col))
    if col > 0:   nodes.append((row, col-1))
    if col < w-1: nodes.append((row, col+1))
    print(f"the valid neighbors for {node} are:", [n for n in nodes if valid(paths, node, n)])
    return [n for n in nodes if valid(paths, node, n)]
# fmt: on


def getpath(paths, node: Node) -> list[Node]:
    path = []
    while node:
        path.append(node)
        node = paths[node]
    return path


# grid = [[int(x) for x in line.strip()] for line in sys.stdin]
grid = [[int(x) for x in line.strip()] for line in open("sample.txt")]
w = len(grid[0])
h = len(grid)

start = (0, 0)
goal = (h - 1, w - 1)
frontier: list[int, Node] = [(0, (0, 0))]
heapq.heapify(frontier)
# start at -1, 0 to simplify the validity check
paths = {start: (-1, 0), (-1, 0): None}
costs = {start: 0}


while len(frontier):
    _, cur = heapq.heappop(frontier)
    if cur == goal:
        break
    for next in nabes(paths, cur):
        print(f"trying {next}")
        pgrid(grid, getpath(paths, cur), next)
        row, col = next
        cost = costs[cur] + grid[row][col]
        if next not in costs or cost < costs[next]:
            costs[next] = cost
            heapq.heappush(frontier, (cost, next))
            paths[next] = cur

path = getpath(paths, goal)
print(path)
pgrid(grid, path)
print(sum(grid[row][col] for (row, col) in path))

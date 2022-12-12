# let's try to use curses to animate our path finding
import curses
import heapq
import time

LOG = open("debug.log", "w")


def debug(string):
    LOG.write(string)
    LOG.flush()


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


def showpathn(stdscr, map_, path):
    for row in range(len(map_)):
        for col in range(len(map_[0])):
            c = chr(ord("a") + map_[row][col])
            if (row, col) in path:
                stdscr.addstr(row, col, c, curses.color_pair(1) | curses.A_BOLD)
            else:
                stdscr.addstr(row, col, c, curses.color_pair(2))


def showcosts(stdscr, map_, costs):
    for row in range(len(map_)):
        for col in range(0, len(map_[0]), 2):
            if (row, col) in costs:
                debug(f"{row} {col} {costs[(row, col)]:^3}\n")
                stdscr.addstr(
                    row,
                    col,
                    f"{costs[(row, col)]:^3}",
                    curses.color_pair(3) | curses.A_BOLD,
                )
            else:
                c = chr(ord("a") + map_[row][col])
                debug(f"{row} {col} {c}\n")
                stdscr.addstr(row, col, f"{c:^3}", curses.color_pair(2))


def showprogress(stdscr, map_, costs):
    for row in range(len(map_)):
        for col in range(0, len(map_[0])):
            c = chr(ord("a") + map_[row][col])
            if (row, col) in costs:
                stdscr.addstr(
                    row,
                    col,
                    c,
                    curses.color_pair(3) | curses.A_BOLD,
                )
            else:
                stdscr.addstr(row, col, c, curses.color_pair(2))


def h(loc, goal):
    return abs(loc[0] - goal[0]) + abs(loc[1] - goal[1]) ** 4


def find(stdscr, map_, start, goal):
    frontier = [(0, start)]
    steps = {start: None}
    costs = {start: 0}
    while frontier:
        _, loc = heapq.heappop(frontier)

        if loc == goal:
            break

        for move in moves(map_, loc):
            cost = costs[loc] + 1 + h(loc, goal)
            if move not in costs or cost < costs[move]:
                costs[move] = cost
                heapq.heappush(frontier, (cost, move))
                steps[move] = loc
        showprogress(stdscr, map_, costs)
        stdscr.refresh()
        curses.napms(0)
    return steps


def findpath(steps, pos):
    path = [pos]
    step = steps[pos]
    while step:
        path.append(step)
        step = steps[step]
    return path


def main(stdscr):
    curses.init_pair(1, curses.COLOR_RED, curses.COLOR_WHITE)
    curses.init_pair(2, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(3, curses.COLOR_GREEN, curses.COLOR_WHITE)

    map_, start, goal = parse(open("input.txt").read().strip())
    steps = find(stdscr, map_, start, goal)
    path = findpath(steps, goal)
    showpathn(stdscr, map_, path)
    stdscr.refresh()
    curses.napms(1000)


if __name__ == "__main__":
    # initialize a curses app and handle cleanup if it dies unexpectedly
    # https://docs.python.org/3/howto/curses.html
    # https://docs.python.org/3/library/curses.html#curses.wrapper
    curses.wrapper(main)

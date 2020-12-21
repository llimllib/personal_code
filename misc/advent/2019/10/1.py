import sys
from copy import deepcopy
from operator import itemgetter

DEBUG = False


def debug(*args):
    if DEBUG:
        print(*args)


def commondivisors(x, y):
    divisors = []
    # if we have a line, we need to check every point along it
    if x == 0:
        return [abs(y)]
    if y == 0:
        return [abs(x)]
    for i in range(2, min(abs(x), abs(y)) + 1):
        if x % i == 0 and y % i == 0:
            divisors.append(i)
    return divisors


def score(comets, y, x):
    visible = 0
    debugmap = {(y,x): 'C'}
    for yy, xx in comets:
        if y == yy and x == xx:
            continue
        dy = yy - y
        dx = xx - x
        divisors = commondivisors(dx, dy)
        if not divisors:
            debugmap[(yy,xx)] = 'o'
            debug(f"{(y, x)} can see {(yy, xx)} (no divisors {(dy, dx)})")
            visible += 1
            continue
        else:
            try:
                for divisor in divisors:
                    vy = dy // divisor if dy else 0
                    vx = dx // divisor if dx else 0
                    i = 1
                    while 1:
                        ix = x + vx * i
                        iy = y + vy * i
                        debug(f"checking {(iy, ix)} {x} + {vx} * {i}")
                        if iy == yy and ix == xx:
                            debugmap[(yy,xx)] = 'o'
                            break
                        if (iy, ix) in comets:
                            debugmap[(yy,xx)] = 'x'
                            debug(f"{(yy, xx)} blocked by {(iy, ix)}")
                            raise StopIteration
                        i += 1
                visible += 1
                debug(f"{(y, x)} can see {(yy, xx)} (dy, dx: {(dy, dx)})")
            except StopIteration:
                continue
    if DEBUG:
        for y, row in enumerate(map_):
            for x, _ in enumerate(row):
                sys.stdout.write(f"{str(debugmap.get((y, x), '_'))}")
            sys.stdout.write("\n")
        sys.stdout.write("\n")
    return visible


def main(map_):
    scores = deepcopy(map_)
    maxscore = (0, 0, 0)
    comets = {}
    for y, row in enumerate(map_):
        for x, _ in enumerate(row):
            if map_[y][x] == "#":
                comets[(y, x)] = 0

    for y, x in comets:
        comets[(y, x)] = score(comets, y, x)

    print(max(comets.items(), key=itemgetter(1)))
    if len(map_) < 11:
        for y, row in enumerate(map_):
            for x, _ in enumerate(row):
                sys.stdout.write(f"{str(comets.get((y, x), 0)):4}")
            sys.stdout.write("\n")


if __name__ == "__main__":
    map_ = []
    for line in open(sys.argv[1]):
        map_.append(list(line.strip()))
    main(map_)

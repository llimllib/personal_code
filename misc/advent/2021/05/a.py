from dataclasses import dataclass

from utils import flatten


@dataclass
class Line:
    x1: int
    y1: int
    x2: int
    y2: int

    def xrange(self):
        if self.x2 > self.x1:
            return range(self.x1, self.x2 + 1)
        else:
            return range(self.x1, self.x2 - 1, -1)

    def yrange(self):
        if self.y2 > self.y1:
            return range(self.y1, self.y2 + 1)
        else:
            return range(self.y1, self.y2 - 1, -1)

    def points(self):
        if self.x1 == self.x2:
            return [(self.x1, y) for y in self.yrange()]
        if self.y1 == self.y2:
            return [(x, self.y1) for x in self.xrange()]
        else:
            return zip(self.xrange(), self.yrange())


def domain(ls):
    """return the maximum x value"""
    return max(flatten((l.x1, l.x2) for l in ls)) + 1


def range_(ls):
    """return the maximum y value"""
    return max(flatten((l.y1, l.y2) for l in ls)) + 1


def makemap(ls):
    return [[0 for _ in range(domain(ls))] for _ in range(range_(ls))]


def parse(f):
    lines = []
    for line in f:
        a, _, b = line.split()

        lines.append(Line(*map(int, a.split(",")), *map(int, b.split(","))))
    return lines


def hv(l):
    """return true if a line is horizontal or vertical"""
    return l.x1 == l.x2 or l.y1 == l.y2


def draw(ls):
    map_ = makemap(ls)

    for l in ls:
        for (x, y) in l.points():
            map_[y][x] += 1
    return map_


def overlaps(map_):
    overlaps = 0
    for row in map_:
        for s in row:
            if s > 1:
                overlaps += 1
    return overlaps


print(overlaps(draw([l for l in parse(open("small.txt")) if hv(l)])))
print(overlaps(draw([l for l in parse(open("input.txt")) if hv(l)])))
print(overlaps(draw(parse(open("small.txt")))))
print(overlaps(draw(parse(open("input.txt")))))

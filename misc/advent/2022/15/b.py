import re


def parse(text):
    return [
        list(map(int, re.findall(r"([\-\d]+)", line)))
        for line in text.split("\n")
        if line
    ]


# https://nedbatchelder.com/blog/201310/range_overlap_in_two_compares.html
def overlap(r1, r2):
    return r1[1] >= r2[0] - 1 and r2[1] >= r1[0] - 1


def merge(r1, r2):
    return (min(r1[0], r2[0]), max(r1[1], r2[1]))


def points(items, row):
    ranges = []
    for sx, sy, bx, by in items:
        dist = abs(sx - bx) + abs(sy - by)
        if not (sy - dist <= row <= sy + dist):
            continue
        delta = dist - abs(sy - row)
        ranges.append((sx - delta, sx + delta))

    # insert into a heap to avoid sort?
    ranges.sort()
    merged = []
    r = ranges[0]
    for i in range(1, len(ranges)):
        if overlap(r, ranges[i]):
            r = merge(r, ranges[i])
        else:
            merged.append(r)
            r = ranges[i]
    merged.append(r)

    return merged


sample = parse(
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""
)
# assert len(points(sample, 10)) == 26, points(sample, 10)

items = parse(open("input.txt").read().strip())

# print("part 1:", len(points(items, 2000000)))
#
# assert search(sample, (0, 0, 20, 20)) == 56000011
#
# print("part 2:", search(items, (0, 0, 4000000, 4000000)))

for i in range(21):
    pts = points(sample, i)
    if len(pts) > 1:
        print(i, pts)

print("----------")
for i in range(4000000):
    pts = points(items, i)
    if len(pts) > 1:
        print(i, pts, ((pts[0][1] + 1) * 4000000) + i)

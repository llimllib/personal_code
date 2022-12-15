import re


def parse(text):
    return [
        list(map(int, re.findall(r"([\-\d]+)", line)))
        for line in text.split("\n")
        if line
    ]


def clamp(x, min_, max_):
    return min(max(min_, x), max_)


def points(items, row, remove_beacons=True, extent=None):
    pts_in_row = set()
    if extent:
        minx, maxx = extent
    for sx, sy, bx, by in items:
        dist = abs(sx - bx) + abs(sy - by)
        if not (sy - dist <= row <= sy + dist):
            continue
        delta = dist - abs(sy - row)
        if extent:
            pts_in_row |= set(
                range(
                    clamp(sx - delta, minx, maxx), clamp(sx + delta + 1, minx, maxx + 1)
                )
            )
        else:
            pts_in_row |= set(range(sx - delta, sx + delta + 1))

    # remove beacons
    if remove_beacons:
        pts_in_row -= {bx for _, _, bx, by in items if by == row}

    return pts_in_row


def search(items, extent):
    mnx, mny, mxx, mxy = extent
    for row in range(mny, mxy):
        pts = points(items, row, False, (mnx, mxx))
        if len(pts) != mxx - mnx + 1:
            for i in range(mnx, mxx + 1):
                if i not in pts:
                    return row + i * 4000000


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
assert len(points(sample, 10)) == 26, points(sample, 10)

items = parse(open("input.txt").read().strip())

print("part 1:", len(points(items, 2000000)))

assert search(sample, (0, 0, 20, 20)) == 56000011

# this takes astronomical time: see b.py
# print("part 2:", search(items, (0, 0, 4000000, 4000000)))

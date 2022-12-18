import math


def parse(text):
    return {
        (int(a), int(b), int(c))
        for a, b, c in [l.split(",") for l in text.strip().split("\n")]
    }


def check(pts):
    exposed = 0
    for x, y, z in pts:
        if (x + 1, y, z) not in pts:
            exposed += 1
        if (x - 1, y, z) not in pts:
            exposed += 1
        if (x, y + 1, z) not in pts:
            exposed += 1
        if (x, y - 1, z) not in pts:
            exposed += 1
        if (x, y, z + 1) not in pts:
            exposed += 1
        if (x, y, z - 1) not in pts:
            exposed += 1
    return exposed


def expand(pts, spot):
    blob = set()
    frontier = [spot]
    while frontier:
        x, y, z = frontier.pop()
        checks = [
            (x + 1, y, z),
            (x - 1, y, z),
            (x, y + 1, z),
            (x, y - 1, z),
            (x, y, z + 1),
            (x, y, z - 1),
        ]
        for chk in checks:
            # in real life, we would need to check the boundaries of the
            # problem; here we'll just arbitrarily enclose it at 0 and 20 on
            # all axes because that is big enough for our samples
            if min(chk) < 0 or max(chk) > 20:
                return blob, False
            if chk not in pts and chk not in blob:
                blob.add(chk)
                frontier.append(chk)
    return blob, True


memo = {True: set(), False: set()}


def enclosed(pts, pt):
    if pt in memo[True]:
        return True
    if pt in memo[False]:
        return False

    blob, is_enclosed = expand(pts, pt)
    memo[is_enclosed] |= blob
    return is_enclosed


def check2(pts):
    exposed = 0
    for x, y, z in pts:
        checks = [
            (x + 1, y, z),
            (x - 1, y, z),
            (x, y + 1, z),
            (x, y - 1, z),
            (x, y, z + 1),
            (x, y, z - 1),
        ]
        for check in checks:
            if check not in pts and not enclosed(pts, check):
                exposed += 1
    return exposed


sample = """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""

assert check(parse(sample)) == 64
print("part 1:", check(parse(open("input.txt").read())))

assert check2(parse(sample)) == 58
print("part 2:", check2(parse(open("input.txt").read())))

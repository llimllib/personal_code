# what if we look at how the values grow?
from collections import defaultdict, Counter


def parse(it):
    start = next(it).strip()
    next(it)
    repls = defaultdict(defaultdict)
    for line in it:
        if not line.strip():
            continue
        a, b = line.strip().split(" -> ")
        repls[a[0]][a[1]] = b
    return start, repls


def diffs(xs):
    diff = []
    for i in range(len(xs) - 1):
        diff.append(xs[i + 1] - xs[i])
    return diff


def sub(start, repls, n):
    bs = []
    for _ in range(n):
        x = []
        for i in range(len(start) - 1):
            x.append(start[i])
            x.append(repls[start[i]][start[i + 1]])
        x.append(start[-1])
        start = x
        c = Counter("".join(start)).most_common()
        bs.append(dict(c)["B"])
    print(bs)
    print(diffs(bs))
    print(diffs(diffs(bs)))
    return "".join(start)


def diff(s):
    counts = Counter(s).most_common()
    return counts[0][1] - counts[-1][1]


start, repls = parse(open("small.txt"))
res = sub(start, repls, 4)
assert res == "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"

print(diff(sub(*parse(open("small.txt")), 10)))
# print(diff(sub(*parse(open("input.txt")), 10)))

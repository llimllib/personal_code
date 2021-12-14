from collections import defaultdict, Counter


def parse(it):
    start = next(it).strip()
    next(it)
    repls = defaultdict(defaultdict)
    for line in it:
        if not line.strip():
            continue
        a, b = line.strip().split(" -> ")
        repls[a] = b
    return start, repls


def iterpairs(s):
    for i in range(len(s) - 1):
        yield s[i : i + 2]


def sub(start, repls, n):
    chars = Counter(start)
    pairs = Counter(iterpairs(start))
    for _ in range(n):
        newpairs = Counter()
        for pair, n in pairs.items():
            r = repls[pair]
            chars[r] += n
            newpairs[pair[0] + r] += n
            newpairs[r + pair[1]] += n
        pairs = newpairs
    common = chars.most_common()
    return common[0][1] - common[-1][1]


print(sub(*parse(open("small.txt")), 10))
print(sub(*parse(open("input.txt")), 10))
print(sub(*parse(open("small.txt")), 40))
print(sub(*parse(open("input.txt")), 40))

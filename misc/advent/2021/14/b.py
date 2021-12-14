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


def buflen(buf):
    return buf.index(None)


def clear(buf):
    for i in range(len(buf)):
        if buf[i]:
            buf[i] = None
        else:
            return


def cp(start, buf):
    for i in range(len(start)):
        buf[i] = start[i]
    # now clear out the rest of the buf
    for i in range(len(start), len(buf)):
        if buf[i]:
            buf[i] = None
        else:
            return


# allocate two large buffers to serve as our scratch
sz = 1024 * 1024 * 1024
buf1 = [None] * sz
buf2 = [None] * sz


def sub(start, repls, n):
    # TODO: mess with sys.intern?
    clear(buf1)
    cp(start, buf2)
    buf = buf1
    src = buf2

    for _ in range(n):
        for i in range(buflen(src) - 1):
            buf[i * 2] = src[i]
            buf[i * 2 + 1] = repls[src[i]][src[i + 1]]
        buf[buflen(buf)] = src[buflen(src) - 1]
        buf, src = src, buf
    return "".join(src[: buflen(src)])


def diff(s):
    counts = Counter(s).most_common()
    return counts[0][1] - counts[-1][1]


start, repls = parse(open("small.txt"))
res = sub(start, repls, 4)
print(res)
assert res == "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"

print(diff(sub(*parse(open("small.txt")), 10)))
print(diff(sub(*parse(open("input.txt")), 10)))
print(diff(sub(*parse(open("small.txt")), 20)))
# print(diff(sub(*parse(open("input.txt")), 40)))

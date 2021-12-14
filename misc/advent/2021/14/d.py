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


def sub(start, repls, n):
    f = open(f"data/00", "w")
    f.write(start)
    f.close()
    for i in range(n):
        fin = open(f"data/{i:02}")
        fout = open(f"data/{i+1:02}", "w")
        while block := fin.read(1024 * 1024):
            for j in range(len(block) - 1):
                fout.write(block[j])
                fout.write(repls[block[j]][block[j + 1]])
            fout.write(block[-1])
        fin.close()
        fout.close()
    return open(f"data/{n:02}")


def diff(fin):
    counter = Counter()
    while block := fin.read(1024 * 1024):
        counter.update(block)
    counts = counter.most_common()
    return counts[0][1] - counts[-1][1]


sub(*parse(open("small.txt")), 4)
print(diff(sub(*parse(open("small.txt")), 10)))
print(diff(sub(*parse(open("input.txt")), 10)))
print(diff(sub(*parse(open("small.txt")), 40)))
print(diff(sub(*parse(open("input.txt")), 40)))

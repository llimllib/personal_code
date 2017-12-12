from functools import reduce
from itertools import cycle, islice, chain
from operator import mul


def take(it, n):
    return iter(list(islice(it, n)))


def revlist(it, start, end):
    return reversed(list(islice(it, start, end)))


def out(it, size):
    print(list(islice(it, size)))


def go(size, ns):
    lst = cycle(range(size))
    pos = 0
    skip = 0
    start = 0
    for n in ns:
        lst = cycle(chain(revlist(lst, 0, n), islice(lst, size - n)))
        take(lst, skip + n)
        start += size - (skip + n)
        pos += skip + n
        skip += 1
    take(lst, start % size)
    print(reduce(mul, islice(lst, 2), 1), pos)


if __name__ == "__main__":
    #go(5, [3, 4, 1, 5])
    go(256, map(int, open("input.txt").read().split(",")))

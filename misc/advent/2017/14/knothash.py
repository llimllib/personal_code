from functools import reduce
from itertools import cycle, islice, chain
from operator import mul, xor


def take(it, n):
    return iter(list(islice(it, n)))


def revlist(it, start, end):
    return reversed(list(islice(it, start, end)))


def out(it, size):
    print(list(islice(it, size)))


def hash(s):
    ns = ordinalize(s)
    size = 256
    lst = cycle(range(size))
    pos = 0
    skip = 0
    start = 0
    for i in range(64):
        for n in ns:
            lst = cycle(chain(revlist(lst, 0, n), islice(lst, size - n)))
            take(lst, skip + n)
            start += size - (skip + n)
            pos += skip + n
            skip += 1
    take(lst, start % size)
    lst = list(take(lst, size))

    dense = []
    while lst:
        group, lst = lst[:16], lst[16:]
        dense.append(reduce(xor, group))
    return ''.join(hex(x)[2:].rjust(2, '0') for x in dense)


def densify(lst):
    assert len(lst) == 256


def go(size, ns):
    lst = cycle(range(size))
    res = hash(lst, 256, ns)
    print(densify(res))


def ordinalize(s):
    return list(map(ord, s)) + [17, 31, 73, 47, 23]


if __name__ == "__main__":
    chars = ordinalize(open("input.txt").read().strip())
    #chars = ordinalize("AoC 2017")
    go(256, chars)

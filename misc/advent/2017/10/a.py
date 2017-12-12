from functools import reduce
from itertools import cycle, islice, chain
from operator import mul


def revlist(it, start, end):
    return reversed(list(islice(it, start, end)))


def go(size, ns):
    lst = cycle(range(size))
    pos = 0
    skip = 0
    for n in ns:
        #import ipdb
        #ipdb.set_trace()
        lst = cycle(chain(revlist(lst, 0, n), islice(lst, size - n)))
        #print(list(islice(lst, size)))
        # skip the next n + skip
        list(islice(lst, n + skip))
        #print(list(islice(lst, size)))
        pos += skip
        skip += 1
    print(reduce(mul, islice(lst, pos, pos + 2), 1), pos)


if __name__ == "__main__":
    go(5, [3, 4, 1, 5])
    go(256, map(int, open("input.txt").read().split(",")))

import itertools
import sys


def part1(ints):
    ls = []
    while any(a != b for a, b in itertools.pairwise(ints)):
        ls.append(ints[-1])
        ints = [b - a for a, b in itertools.pairwise(ints)]
    return sum([ints[0]] + ls)


in1, in2 = itertools.tee(sys.stdin)
print(sum(part1([int(x) for x in line.split()]) for line in in1))
print(sum(part1(list(reversed([int(x) for x in line.split()]))) for line in in2))

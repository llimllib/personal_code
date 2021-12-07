import sys
import itertools

from cpu import cpu

input_file = sys.argv[1]
mem = [int(n) for n in open(input_file).read().strip().split(",")]


def run(signal):
    a = cpu(mem, [signal[0], 0])
    b = cpu(mem, [signal[1], a])
    c = cpu(mem, [signal[2], b])
    d = cpu(mem, [signal[3], c])
    return cpu(mem, [signal[4], d])


max_ = 0
for perm in itertools.permutations(range(5)):
    val = int(run(perm))
    if val > max_:
        print(max_)
        max_ = val

print(max_)

import sys
import itertools
import ipdb

from cpu2 import cpu

input_file = sys.argv[1]
mem = [int(n) for n in open(input_file).read().strip().split(",")]


def run(mem, signals):
    a = cpu(mem[:], [signals[0], 0])
    b = cpu(mem[:], [signals[1], next(a)])
    c = cpu(mem[:], [signals[2], next(b)])
    d = cpu(mem[:], [signals[3], next(c)])
    e = cpu(mem[:], [signals[4], next(d)])
    ev = next(e)
    lastval = ev
    while 1:
        print(f"loop: {ev}")
        av = a.send(ev)
        bv = b.send(av)
        cv = c.send(bv)
        dv = d.send(cv)
        ev = e.send(dv)
        if ev:
            lastval = ev
        else:
            print("breaking")
            break

    return lastval


# max_ = 0
# for perm in itertools.permutations(range(5, 10)):
#     val = int(run(perm))
#     if val > max_:
#         print(max_)
#         max_ = val
#
# print(max_)

print(run(mem, [9, 8, 7, 6, 5]))
# print(list(cpu(mem, [5])))

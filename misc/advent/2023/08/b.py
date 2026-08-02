import itertools
import numpy as np
import re
import sys


def parse(iter):
    directions = [1 if c == "R" else 0 for c in list(next(iter).strip())]
    next(iter)
    network = {}
    for line in iter:
        a, l, r = re.findall(r"\w+", line)
        network[a] = (l, r)
    return (directions, network)


def cycle(directions, network, node):
    loc = node
    for i, dir in enumerate(itertools.cycle(directions)):
        loc = network[loc][dir]
        if loc[-1] == "Z":
            return i + 1


def run(directions, network):
    cycles = [cycle(directions, network, node) for node in network if node[-1] == "A"]
    return np.lcm.reduce(cycles)


print(run(*parse(sys.stdin)))

import sys
import ipdb
from math import *

f = sys.argv[1] if len(sys.argv) > 1 else "input.txt"
lines = [l for l in open(f)]

timestamp = int(lines[0])
busids = [int(l) for l in lines[1].split(",") if l != "x"]
mins = [0 for i in range(len(busids))]

ns = None
i = 1
while any(m == 0 for m in mins):
    ns = list(map(lambda b: i * b, busids))
    for nidx, n in enumerate(ns):
        if n > timestamp and mins[nidx] == 0:
            mins[nidx] = n
            print(mins)
    i += 1

mn = min(mins)
idx = mins.index(mn)
print(mn, i, idx, busids[idx], (mn - timestamp) * busids[idx])

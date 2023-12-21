from collections import defaultdict
import sys

garden = set()
gnomes = set()
for row, line in enumerate(sys.stdin):
    for col, c in enumerate(line):
        if c == "." or c == "S":
            garden.add(complex(col, row))
        if c == "S":
            gnomes.add(complex(col, row))

N, E, S, W = 0 - 1j, 1 + 0j, 0 + 1j, -1 + 0j
adj = defaultdict(list)
for point in garden:
    for delta in [N, E, S, W]:
        if point + delta in garden:
            adj[point].append(point + delta)

for i in range(64):
    gs = set()
    for gnome in gnomes:
        gs.update(adj[gnome])
    gnomes = gs

print(len(gnomes))

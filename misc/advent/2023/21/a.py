import sys

garden = set()
gnomes = set()
for row, line in enumerate(sys.stdin):
    for col, c in enumerate(line):
        if c == "." or c == "S":
            garden.add(complex(col, row))
        if c == "S":
            gnomes.add(complex(col, row))

N, E, S, W = -1j, 1 + 0j, 1j, -1 + 0j
adj = {
    point: [point + delta for delta in [N, E, S, W] if point + delta in garden]
    for point in garden
}

for i in range(64):
    gs = set()
    for gnome in gnomes:
        gs.update(adj[gnome])
    gnomes = gs

print(len(gnomes))

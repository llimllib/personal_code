import ipdb
import sys

garden = set()
gnomes = set()
h = 0
w = 0
# for row, line in enumerate(sys.stdin):
for row, line in enumerate(open("sample.txt")):
    for col, c in enumerate(line.strip()):
        if c == "." or c == "S":
            garden.add(complex(col, row))
        if c == "S":
            gnomes.add(complex(col, row))
        w = col
    h = row

w += 1
h += 1

N, E, S, W = -1j, 1 + 0j, 1j, -1 + 0j
adj = {
    point: [
        ẟ
        for ẟ in [N, E, S, W]
        if point + ẟ in garden
        or (point + ẟ).real < 0  # helpfully, the border of the map is comprised
        or (point + ẟ).imag < 0  # of only garden points. So we can assume that
        or (point + ẟ).real > w  # all boundary points are garden squares
        or (point + ẟ).imag > h
    ]
    for point in garden
}


def normalize(c):
    return complex(c.real % w, c.imag % h)


for i in range(5000):
    if i in (6, 10, 50, 100, 501, 1001):
        print(f"{i}: {len(gnomes)}")
    gs = set()
    for gnome in gnomes:
        col = gnome.real % w
        row = gnome.imag % h
        # print(gnome, m, row, col, [gnome + x for x in adj[complex(col, row)]])
        # if any(x * (m + 1) not in garden for x in adj[complex(row, col)]):
        #     ipdb.set_trace()
        # print(gnome)
        # ipdb.set_trace()
        gs.update([gnome + x for x in adj[complex(col, row)]])
        # if any(normalize(g) not in adj for g in gs):
        #     ipdb.set_trace()
        #     print("hi")
    gnomes = gs
    print(gnomes)

print(len(gnomes))

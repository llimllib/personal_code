def move(h, t, d):
    dx, dy = d
    h = (h[0] + dx, h[1] + dy)
    if t[0] == h[0] and abs(t[1] - h[1]) == 2:
        t = (t[0], t[1] + (1 if t[1] < h[1] else -1))
    elif t[1] == h[1] and abs(t[0] - h[0]) == 2:
        t = (t[0] + (1 if t[0] < h[0] else -1), t[1])
    elif abs(t[0] - h[0]) + abs(t[1] - h[1]) > 2:
        t = (t[0] + (1 if t[0] < h[0] else -1), t[1] + (1 if t[1] < h[1] else -1))

    return h, t


moves = {
    "R": (1, 0),
    "L": (-1, 0),
    "U": (0, -1),
    "D": (0, 1),
}


def sim(cmds):
    h = (5, 5)
    t = (5, 5)
    visited = []
    for cmd in cmds:
        dir, n = cmd.split()
        n = int(n)
        for _ in range(n):
            h, t = move(h, t, moves[dir])
            visited.append(t)
    print(f"tail visited: {len(set(visited))}")


def sim10(cmds):
    knots = [(5, 5) for _ in range(10)]
    visited = []
    for cmd in cmds:
        dir, n = cmd.split()
        n = int(n)
        for _ in range(n):
            h, t = move(knots[0], knots[1], moves[dir])
            knots[0] = h
            knots[1] = t
            for i in range(1, len(knots) - 1):
                h, t = move(knots[i], knots[i + 1], (0, 0))
                knots[i] = h
                knots[i + 1] = t
            visited.append(knots[-1])
    print(f"tail visited: {len(set(visited))}")


sample = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""
sim(sample.split("\n"))
sim(open("input.txt").read().strip().split("\n"))

sample2 = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"""

sim10(sample.split("\n"))
sim10(sample2.split("\n"))
sim10(open("input.txt").read().strip().split("\n"))

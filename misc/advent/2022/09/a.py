moves = {
    "R": (1, 0),
    "L": (-1, 0),
    "U": (0, -1),
    "D": (0, 1),
}


def clamp(x, min_, max_):
    return min(max(min_, x), max_)


def adjust(h, t):
    dx, dy = h[0] - t[0], h[1] - t[1]
    if abs(dx) >= 2 or abs(dy) >= 2:
        return (
            t[0] + clamp(dx, -1, 1),
            t[1] + clamp(dy, -1, 1),
        )
    return t


def simn(cmds, n_knots):
    knots = [(5, 5) for _ in range(n_knots)]
    visited = set()
    for cmd in cmds:
        dir, n = cmd.split()
        n = int(n)
        dx, dy = moves[dir]
        for _ in range(n):
            knots[0] = (knots[0][0] + dx, knots[0][1] + dy)
            for i in range(1, len(knots)):
                knots[i] = adjust(knots[i - 1], knots[i])
            visited.add(knots[-1])
    return len(visited)


sample = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""
assert simn(sample.split("\n"), 2) == 13, simn(sample.split("\n"), 2)
print(simn(open("input.txt").read().strip().split("\n"), 2))

sample2 = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"""

assert simn(sample.split("\n"), 10) == 1
assert simn(sample2.split("\n"), 10) == 36
print(simn(open("input.txt").read().strip().split("\n"), 10))

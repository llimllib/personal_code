import sys


def parse(text):
    elves = set()
    lines = text.strip().split("\n")
    for row in range(len(lines)):
        for col in range(len(lines[row])):
            if lines[row][col] == "#":
                elves.add(complex(row, col))
    return elves


NEIGHBORS = (-1 + -1j, -1, -1 + 1j, -1j, 1j, 1 - 1j, 1, 1 + 1j)


def neighbors(elves, e):
    return set(e + n for n in NEIGHBORS if e + n in elves)


NW = -1 - 1j
N = -1
NE = -1 + 1j
W = -1j
E = 1j
SW = 1 - 1j
S = 1
SE = 1 + 1j


def propN(neighbors, e):
    for n in (NW, N, NE):
        if e + n in neighbors:
            return None
    return e + N


def propS(neighbors, e):
    for n in (SW, S, SE):
        if e + n in neighbors:
            return None
    return e + S


def propW(neighbors, e):
    for n in (NW, W, SW):
        if e + n in neighbors:
            return None
    return e + W


def propE(neighbors, e):
    for n in (NE, E, SE):
        if e + n in neighbors:
            return None
    return e + E


def proposal(neighbors, e, rnd):
    if not neighbors:
        return 0

    props = [propN, propS, propW, propE]
    for prop in range(len(props)):
        p = props[(prop + rnd) % 4](neighbors, e)
        if p:
            return p
    return e


def pelves(elves):
    minrow = int(min(e.real for e in elves))
    maxrow = int(max(e.real for e in elves))
    mincol = int(min(e.imag for e in elves))
    maxcol = int(max(e.imag for e in elves))
    for row in range(minrow, maxrow + 1):
        for col in range(mincol, maxcol + 1):
            if complex(row, col) in elves:
                sys.stdout.write("#")
            else:
                sys.stdout.write(".")
        sys.stdout.write("\n")


def run(elves, rounds=1):
    round = 0
    for rnd in range(rounds):
        round += 1
        props = {}
        proposals = set()
        conflicts = set()
        for e in elves:
            prop = proposal(neighbors(elves, e), e, rnd)
            if prop in proposals:
                conflicts.add(prop)
                props[e] = prop
            else:
                proposals.add(prop)
                props[e] = prop

        # now go through the props and remove any that became conflicted
        for elf, prop in props.items():
            if prop in conflicts:
                props[elf] = elf

        new_elves = set(props.values())
        if elves == new_elves:
            break
        elves = new_elves
    print(len(elves))
    minrow = int(min(e.real for e in elves))
    maxrow = int(max(e.real for e in elves))
    mincol = int(min(e.imag for e in elves))
    maxcol = int(max(e.imag for e in elves))
    return ((maxrow - minrow + 1) * (maxcol - mincol + 1)) - len(elves), round


sample = """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"""
assert run(parse(sample), 10)[0] == 110
assert run(parse(sample), 40)[1] == 20

print("part 1:", run(parse(open("input.txt").read()), 10)[0])
# 925 is a wrong guess. How?
print("part 2:", run(parse(open("input.txt").read()), 100_000)[1])

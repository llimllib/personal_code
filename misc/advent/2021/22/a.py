import re

ON = 0
OFF = 1


def fifty(n):
    return n >= -50 and n <= 50


def valid(cmd):
    # The initialization procedure only uses cubes that have x, y, and z
    # positions of at least -50 and at most 50. For now, ignore cubes
    # outside this region.
    _, (x1, x2), (y1, y2), (z1, z2) = cmd
    return all(map(fifty, [x1, x2, y1, y2, z1, z2]))


def parse(it):
    cmds = []
    for line in it:
        line = line.strip()
        if not line:
            continue
        op, _, x1, x2, _, y1, y2, _, z1, z2 = re.split(r" |=|,|\.\.", line)
        cmds.append(
            (
                ON if op == "on" else OFF,
                (int(x1), int(x2)),
                (int(y1), int(y2)),
                (int(z1), int(z2)),
            )
        )
    cmds = filter(valid, cmds)
    return cmds


def do(cmds):
    cubes = set()
    for op, (x1, x2), (y1, y2), (z1, z2) in cmds:
        if op == ON:
            for x in range(x1, x2 + 1):
                for y in range(y1, y2 + 1):
                    for z in range(z1, z2 + 1):
                        cubes.add((x, y, z))
        else:
            for x in range(x1, x2 + 1):
                for y in range(y1, y2 + 1):
                    for z in range(z1, z2 + 1):
                        try:
                            cubes.remove((x, y, z))
                        except KeyError:
                            pass
    return cubes


if __name__ == "__main__":
    assert len(do(parse(open("small.txt")))) == 39
    assert len(do(parse(open("medium.txt")))) == 590784
    print(len(do(parse(open("input.txt")))))

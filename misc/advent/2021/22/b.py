import re

import ipdb

ON = 0
OFF = 1


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
    return cmds


def overlap(x1, x2, xx1, xx2) -> bool:
    return max(x1, x2) > min(xx1, xx2) and min(x1, x2) < max(xx1, xx2)


def intersect(cubes, cube) -> bool:
    x1, x2, y1, y2, z1, z2 = cube
    for cx1, cx2, cy1, cy2, cz1, cz2 in cubes:
        if (
            overlap(x1, x2, cx1, cx2)
            and overlap(y1, y2, cy1, cy2)
            and overlap(z1, z2, cz1, cz2)
        ):
            return True
    return False


def subtract(cubes, cube):
    ipdb.set_trace()
    print("TODO", cube)


def do(cmds):
    cubes = set()
    for op, (x1, x2), (y1, y2), (z1, z2) in cmds:
        if op == ON:
            cubes.add((x1, x2, y1, y2, z1, z2))
        else:
            cube = (x1, x2, y1, y2, z1, z2)
            if intersect(cubes, cube):
                subtract(cubes, cube)
    return cubes


if __name__ == "__main__":
    print(len(do(parse(open("medium.txt")))))
    print(len(do(parse(open("input.txt")))))

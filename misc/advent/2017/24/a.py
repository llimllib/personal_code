from collections import defaultdict
from copy import copy


def remove(bridges, bridge):
    bridges = copy(bridges)
    bridges.remove(bridge)
    return bridges


def find(bridges, connection):
    return [b for b in bridges if connection in b]


def strip(part, n):
    if part[0] == n: return part[1]
    return part[0]


def append(bridge, part):
    bridge = copy(bridge)
    bridge.append(part)
    return bridge


def flatten(list_of_bridges):
    bridges = []

    # handle the [(a,b)] casee
    if isinstance(list_of_bridges[0], tuple):
        return list_of_bridges

    while len(list_of_bridges):
        bridge = list_of_bridges.pop(0)
        if isinstance(bridge[0], tuple):
            bridges.append(bridge)
        else:
            for bridge in bridge:
                list_of_bridges.append(bridge)
    return bridges


def search(parts, bridge, nextpart):
    if not find(parts, nextpart):
        return [bridge]
    else:
        return [
            search(
                remove(parts, part),
                append(bridge, part), strip(part, nextpart))
            for part in find(parts, nextpart)
        ]


def weight(bridge):
    try:
        return sum(sum(part) for part in bridge)
    except TypeError:
        print(bridge)
        raise


def go(inp):
    parts = []
    for part in inp:
        parts.append(tuple(map(int, part.split(r"/"))))

    bridges = []
    for start in find(parts, 0):
        bridges_ = list(search(remove(parts, start), [start], strip(start, 0)))
        bridges.append(bridges_)

    bridges = flatten(bridges)
    print(max((weight(bridge), bridge) for bridge in bridges))
    print(max((len(bridge), weight(bridge), bridge) for bridge in bridges))


if __name__ == "__main__":
    sample = """0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10""".split("\n")
    #go(sample)
    go(open('input.txt'))

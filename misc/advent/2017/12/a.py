from collections import defaultdict


def go(inp):
    comms = defaultdict(set)
    for line in inp:
        p, rest = line.split("<->")
        p = int(p)

        ps = map(int, rest.split(","))
        comms[p].update(ps)

    connected = set()
    frontier = comms[0]
    del comms[0]
    groups = 0
    while 1:
        f = set()
        for partner in frontier:
            if partner not in connected:
                connected.add(partner)
                f.update(comms[partner])
                del comms[partner]
        if not f:
            if not comms:
                groups += 1
                break
            if groups == 0:
                print(len(connected))
            groups += 1
            nextkey = list(comms.keys())[0]
            f = comms[nextkey]
            connected = set()
            del comms[nextkey]
        frontier = f

    print(groups)


if __name__ == "__main__":
    go(open("input.txt"))

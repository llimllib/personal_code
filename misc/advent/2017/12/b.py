from collections import defaultdict


def go(inp):
    comms = defaultdict(set)
    for line in inp:
        p, rest = line.split("<->")
        p = int(p)
        ps = map(int, rest.split(","))
        comms[p].update(ps)

    connected = set()
    frontier = list(comms[0])
    del comms[0]
    groups = 0
    while 1:
        partner = frontier.pop(0)
        if partner not in connected:
            connected.add(partner)
            frontier += list(comms[partner])
            del comms[partner]
        if not frontier:
            if not comms:
                groups += 1
                break
            if groups == 0:
                print(len(connected))
            groups += 1
            nextkey = list(comms.keys())[0]
            frontier = list(comms[nextkey])
            connected = set()
            del comms[nextkey]

    print(groups)


if __name__ == "__main__":
    go(open("input.txt"))

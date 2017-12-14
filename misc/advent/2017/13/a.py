from collections import defaultdict
from copy import deepcopy
import sys


def advance(fw):
    for level in fw:
        rule = fw[level]
        if not rule:
            continue
        try:
            idx = rule.index(1)
            rule[idx] = 0
            if idx < len(rule) - 1:
                rule[idx + 1] = 1
            else:
                rule[idx - 1] = -1
        except ValueError:
            idx = rule.index(-1)
            rule[idx] = 0
            if idx > 0:
                rule[idx - 1] = -1
            else:
                rule[idx + 1] = 1


def go(inp):
    fw = defaultdict(list)
    for line in inp:
        n1, n2 = line.split(":")
        fw[int(n1)] = [1] + [0] * (int(n2) - 1)

    level = 0
    penalty = 0
    maxlev = max(fw.keys())
    while level <= maxlev:
        if fw[level] and fw[level][0] != 0:
            penalty += level * len(fw[level])
        advance(fw)
        level += 1
    print(penalty)


# Works but too slow!
def go2(inp):
    firewall = defaultdict(list)
    for line in inp:
        n1, n2 = line.split(":")
        firewall[int(n1)] = [1] + [0] * (int(n2) - 1)

    delay = 0
    while 1:
        sys.stdout.write(".")
        sys.stdout.flush()
        fw = deepcopy(firewall)
        delay += 1
        for _ in range(delay):
            advance(fw)
        try:
            level = 0
            maxlev = max(fw.keys())
            while level <= maxlev:
                if fw[level] and fw[level][0] != 0:
                    raise ValueError("got got")
                advance(fw)
                level += 1
            break
        except ValueError:
            continue

    print(delay)


def isclosed(n, step):
    return step % (2 * (n - 1)) == 0


def go3(inp):
    firewall = defaultdict(list)
    for line in inp:
        n1, n2 = line.split(":")
        firewall[int(n1)] = int(n2)
    maxlev = max(firewall.keys())

    delay = 0
    while 1:
        ts = range(delay, delay + maxlev + 1)
        try:
            for i, val in enumerate(ts):
                if firewall[i] and isclosed(firewall[i], val):
                    raise ValueError("got got")
            print(delay)
            break
        except ValueError:
            pass
        delay += 1


if __name__ == "__main__":
    go(open("input.txt"))
    go3(open("input.txt"))

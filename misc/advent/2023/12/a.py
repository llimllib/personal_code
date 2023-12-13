import sys


def parse(iter):
    lines = []
    for line in iter:
        cond, lens = line.strip().split(" ")
        lines.append((cond, [int(x) for x in lens.split(",")]))
    return lines


def count(s: str, machines: list[int]) -> int:
    if not machines:
        if "#" not in s:
            return 1
        return 0
    if not s:
        return 0

    i = 0
    l = len(s)
    m = machines[0]
    n = 0
    while i < l + m:
        match = i
        while i < l and s[i] in ["?", "#"] and i - match < m:
            i += 1
        if i - match == m and (i == l or s[i] in [".", "?"]) and "#" not in s[:match]:
            n += count(
                s[i + 1 :],
                machines[1:],
            )
        i = match + 1
    return n


print(sum(count(s, machines) for (s, machines) in parse(sys.stdin)))

import functools
import sys


def parse(iter):
    lines = []
    for line in iter:
        cond, lens = line.strip().split(" ")
        lines.append((cond, tuple(int(x) for x in lens.split(","))))
    return lines


@functools.cache
def count(s: str, machines: tuple[int]) -> int:
    if not machines:
        if "#" not in s:
            return 1
        return 0

    i = 0
    l = len(s)
    m = machines[0]
    n = 0
    while i < l + m and "#" not in s[:i]:
        match = i
        while i < l and s[i] in ["?", "#"] and i - match < m:
            i += 1
        if i - match == m and (i == l or s[i] in [".", "?"]):
            n += count(s[i + 1 :], machines[1:])
        i = match + 1
    return n


def unfold(s: str, machines: tuple[int]) -> tuple[str, tuple[int]]:
    return "?".join([s] * 5), machines * 5


puzzles = parse(sys.stdin)
# part 1
print(sum(count(s, machines) for (s, machines) in puzzles))
# part 2
print(sum(count(*unfold(*x)) for x in puzzles))

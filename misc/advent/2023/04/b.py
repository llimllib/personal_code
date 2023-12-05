from collections import defaultdict
import re
import sys


def parse(line):
    front, mine = line.split("|")
    _, winners = front.split(":")
    return [
        set(map(int, re.findall(r"\d+", winners))),
        set(map(int, re.findall(r"\d+", mine))),
    ]


bonus = defaultdict(int)

for i, line in enumerate(sys.stdin):
    winners, mine = parse(line)
    n = len(winners & mine)
    bonus[i] += 1
    for k in range(i + 1, i + n + 1):
        bonus[k] += bonus[i]

print(sum(bonus.values()))

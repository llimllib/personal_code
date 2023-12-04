import re
import sys


def parse(line):
    front, mine = line.split("|")
    _, winners = front.split(":")
    return [
        set(map(int, re.findall(r"\d+", winners))),
        set(map(int, re.findall(r"\d+", mine))),
    ]


def score(a, b):
    intersection = a & b
    return 2 ** (len(intersection) - 1) if intersection else 0


print(sum(score(*parse(line)) for line in sys.stdin))

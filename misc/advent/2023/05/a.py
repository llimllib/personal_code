import re
import sys


def parse():
    chunks = sys.stdin.read().strip().split("\n\n")
    seeds = [int(x) for x in re.findall(r"\d+", chunks.pop(0))]

    intervals = []
    for chunk in chunks:
        map = []
        for line in chunk.split("\n")[1:]:
            dest, source, n = [int(x) for x in re.findall(r"\d+", line)]
            map.append(((source, source + n - 1), dest - source))
        intervals.append(map)

    return (seeds, intervals)


def run(seed: int, intervals: list[list[tuple[tuple[int, int], int]]]) -> int:
    for map in intervals:
        for (a, b), adj in map:
            if a <= seed <= b:
                seed += adj
                break
    return seed


seeds, intervals = parse()
print(min(run(s, intervals) for s in seeds))

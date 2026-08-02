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


def run(
    seed: int, intervals: list[list[tuple[tuple[int, int], int]]], debug=False
) -> int:
    if debug:
        sys.stdout.write(f"{seed}")
    for map in intervals:
        for (a, b), adj in map:
            if a <= seed <= b:
                seed += adj
                break
        if debug:
            sys.stdout.write(f" -> {seed}")
    if debug:
        print()
    return seed


# I'm pretty sure this part is correct, but will take until the heat death of
# the universe to complete
seeds, intervals = parse()

# print(
#     min(
#         min(run(i, intervals) for i in range(a, a + b))
#         for a, b in list(zip(seeds[::2], seeds[1::2]))
#     )
# )

for a, b in list(zip(seeds[::2], seeds[1::2])):
    for i in range(a, a + b):
        print(i, run(i, intervals))
    print("----")

from pprint import pprint

pprint(intervals)

run(82, intervals, True)

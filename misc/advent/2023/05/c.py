import ipdb
from pprint import pprint
import re
import sys


Range = tuple[int, int]
Transform = tuple[Range, int]
Atlas = list[Transform]


def parse() -> tuple[list[int], list[Atlas]]:
    # DELTEME
    chunks = open("sample.txt").read().strip().split("\n\n")
    # chunks = sys.stdin.read().strip().split("\n\n")
    seeds = [int(x) for x in re.findall(r"\d+", chunks.pop(0))]

    intervals = []
    for chunk in chunks:
        map = []
        for line in chunk.split("\n")[1:]:
            dest, source, n = [int(x) for x in re.findall(r"\d+", line)]
            map.append(((source, source + n - 1), dest - source))
        intervals.append(map)

    return (seeds, intervals)


def overlap(t: Transform, t2: Transform) -> bool:
    """
    True iff t overlaps t2

    https://nedbatchelder.com/blog/201310/range_overlap_in_two_compares.html
    """
    return t[0][1] >= t2[0][0] and t2[0][1] >= t[0][0]


def merge_(t1: Transform, t2: Transform) -> list[Transform]:
    transforms = []

    a, b = sorted([t1, t2], key=lambda x: x[0])
    (start1, end1), adj1 = a
    (start2, end2), adj2 = b

    if start1 < start2:
        transforms.append(((start1, start2 - 1), adj1))
    transforms.append(((start2, min(end1, end2)), adj1 + adj2))
    if end1 > end2:
        transforms.append(((end2 + 1, end1), adj1))
    elif end2 > end1:
        transforms.append(((end1 + 1, end2), adj2))

    print(f"merge({t1}, {t2}) ->")
    print(f"    {transforms}")

    return transforms


def merge(atlas: Atlas, piece: Transform) -> Atlas:
    """given a sorted atlas, merge the piece"""
    newatlas = []
    for t in atlas:
        if overlap(t, piece):
            pieces = merge_(t, piece)
            piece = pieces.pop()
            newatlas.extend(pieces)
        else:
            newatlas.append(t)
    newatlas.append(piece)

    newatlas.sort()
    return newatlas


def run(atlas: Atlas, seed: int) -> int:
    for (a, b), adj in atlas:
        if a <= seed <= b:
            return seed + adj
    return seed


seeds, intervals = parse()


# let's sum up the transformations so we get a one-layer transformation map,
# and see what that looks like
atlas = intervals.pop(0)
atlas.sort()
pprint(atlas)
print("----")
# for at in intervals[:2]:
for at in intervals:
    for piece in at:
        atlas = merge(atlas, piece)
        print(piece)
        pprint(atlas)
        print("----")

print(run(atlas, 82))

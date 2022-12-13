from functools import cmp_to_key
import json


def lst(a):
    return [a] if not isinstance(a, list) else a


def cmp(a, b) -> int:
    if a == b:
        return 0
    if isinstance(a, int) and isinstance(b, int):
        if a > b:
            return 1
        if a < b:
            return -1
        return 0
    elif isinstance(a, list) and isinstance(b, list):
        if not a:
            return -1
        if not b:
            return 1
        if (res := cmp(a[0], b[0])) != 0:
            return res
        return cmp(a[1:], b[1:])
    # all that's left is the mixed int-list case
    if not lst(a):
        return -1
    if not lst(b):
        return 1
    if (res := cmp(lst(a)[0], lst(b)[0])) != 0:
        return res
    return cmp(lst(a)[1:], lst(b)[1:])


def ordered(a, b) -> bool:
    return cmp(a, b) == -1


assert ordered([1, 1, 3, 1, 1], [1, 1, 5, 1, 1])
assert ordered([[1], [2, 3, 4]], [[1], 4])
assert not ordered([9], [[8, 7, 6]])
assert ordered([[4, 4], 4, 4], [[4, 4], 4, 4, 4])
assert not ordered([7, 7, 7, 7], [7, 7, 7])
assert ordered([], [3])
assert not ordered([[[]]], [[]])
assert not ordered(
    [1, [2, [3, [4, [5, 6, 7]]]], 8, 9], [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]
)

print("part 1:")

pairs = [
    [json.loads(ll) for ll in l.split("\n")]
    for l in open("sample.txt").read().strip().split("\n\n")
]
print(
    "    sample: ",
    sum(i + 1 for i in range(len(pairs)) if ordered(pairs[i][0], pairs[i][1])),
)

pairs = [
    [json.loads(ll) for ll in l.split("\n")]
    for l in open("input.txt").read().strip().split("\n\n")
]
print(
    "    input: ",
    sum(i + 1 for i in range(len(pairs)) if ordered(pairs[i][0], pairs[i][1])),
)

print("part 2:")

s1, s2 = [[[2]], [[6]]]
lists = [
    json.loads(l) for l in open("sample.txt").read().strip().split("\n") if l.strip()
] + [s1, s2]
lists.sort(key=cmp_to_key(cmp))
print("    sample:", (lists.index(s1) + 1) * (lists.index(s2) + 1))

lists = [
    json.loads(l) for l in open("input.txt").read().strip().split("\n") if l.strip()
] + [s1, s2]
lists.sort(key=cmp_to_key(cmp))
print("    input:", (lists.index(s1) + 1) * (lists.index(s2) + 1))

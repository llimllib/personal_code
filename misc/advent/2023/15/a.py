from collections import defaultdict
import sys


def hash(s: str) -> int:
    sum = 0
    for c in s:
        sum = ((sum + ord(c)) * 17) % 256
    return sum


instructions = sys.stdin.read().strip().split(",")
print("part 1:", sum(hash(i) for i in instructions))

boxes = defaultdict(dict)
for i in instructions:
    if "-" in i:
        try:
            del boxes[hash(i[:-1])][i[:-1]]
        except KeyError:
            pass
    else:
        label, val = i.split("=")
        val = int(val)
        box = hash(label)
        boxes[box][label] = val

print(
    "part 2:",
    sum(
        (box + 1) * (i + 1) * foc
        for box in boxes
        for i, foc in enumerate(boxes[box].values())
    ),
)

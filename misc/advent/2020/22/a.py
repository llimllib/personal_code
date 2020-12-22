from operator import mul
from functools import reduce

p1, p2 = open("small.txt").read().split("\n\n")
p1, p2 = open("input.txt").read().split("\n\n")
p1 = [int(l) for l in p1.strip().split("\n")[1:]]
p2 = [int(l) for l in p2.strip().split("\n")[1:]]

while p1 and p2:
    c1 = p1.pop(0)
    c2 = p2.pop(0)
    p1.extend([c1, c2]) if c1 > c2 else p2.extend([c2, c1])

win = p1 if p1 else p2
print(f"part 1: {sum(a*b for a,b in zip(reversed(win), range(1,len(win)+1)))}")


def rc(p1, p2):
    history = set()

    while p1 and p2:
        key = (tuple(p1), tuple(p2))
        key = "".join(p1) + "|" + "".join(p2)
        if key in history:
            return 1
        history.add(key)
        c1 = p1.pop(0)
        c2 = p2.pop(0)
        if len(p1) >= c1 and len(p2) >= c2:
            result = rc(p1[:c1], p2[:c2])
            p1.extend([c1, c2]) if result == 1 else p2.extend([c2, c1])
        else:
            p1.extend([c1, c2]) if c1 > c2 else p2.extend([c2, c1])
    return 1 if p1 else 2


p1, p2 = open("small.txt").read().split("\n\n")
p1, p2 = open("input.txt").read().split("\n\n")
p1 = [int(l) for l in p1.strip().split("\n")[1:]]
p2 = [int(l) for l in p2.strip().split("\n")[1:]]
rc(p1, p2)
win = p1 if p1 else p2
print(f"part 2: {sum(a*b for a,b in zip(reversed(win), range(1,len(win)+1)))}")

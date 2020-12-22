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
        if (p1, p2) in history:
            return 1, p1, p2
        history.add((p1, p2))
        c1, p1 = p1[0], p1[1:]
        c2, p2 = p2[0], p2[1:]
        if len(p1) >= c1 and len(p2) >= c2:
            result, _, _ = rc(p1[:c1], p2[:c2])
            if result == 1:
                p1 += (c1, c2)
            else:
                p2 += (c2, c1)
        else:
            if c1 > c2:
                p1 += (c1, c2)
            else:
                p2 += (c2, c1)
    return 1 if p1 else 2, p1, p2


# p1, p2 = open("small.txt").read().split("\n\n")
p1, p2 = open("input.txt").read().split("\n\n")
p1 = tuple(int(l) for l in p1.strip().split("\n")[1:])
p2 = tuple(int(l) for l in p2.strip().split("\n")[1:])
_, p1, p2 = rc(p1, p2)
win = p1 if p1 else p2
print(f"part 2: {sum(a*b for a,b in zip(reversed(win), range(1,len(win)+1)))}")

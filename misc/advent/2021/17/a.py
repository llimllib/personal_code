from typing import Tuple


def run(xtarget, ytarget, xvel, yvel, debug=False) -> Tuple[bool, int]:
    x, y = 0, 0
    maxx = max(xtarget)
    miny = min(ytarget)
    maxh = y
    while x <= maxx and y >= miny:
        if debug:
            print(f"x={x}, y={y}, xvel={xvel}, yvel={yvel}, maxx={maxx}, miny={miny}")
        if xtarget[0] <= x <= xtarget[1] and ytarget[0] <= y <= ytarget[1]:
            return (True, maxh)
        x += xvel
        y += yvel
        if y > maxh:
            maxh = y
        xvel -= 1 if xvel > 0 else 0  # does not handle negatives
        yvel -= 1
    return (False, -1)


def maxh(xtarget, ytarget):
    return max(
        h
        for hit, h in [
            run(xtarget, ytarget, xvel, yvel)
            for yvel in range(abs(min(ytarget)))
            for xvel in range(max(xtarget) // 4)
        ]
        if hit
    )


def findall(xtarget, ytarget):
    hits = []
    for y in range(min(ytarget), abs(min(ytarget)) + 1):
        for x in range(abs(max(xtarget)) + 1):
            hit, _ = run(xtarget, ytarget, x, y)
            if hit:
                hits.append((x, y))

    return hits


# sample input: target area: x=20..30, y=-10..-5

sample = ((20, 30), (-10, -5))
assert run(*sample, 9, 0)[0]
assert run(*sample, 7, 2)[0]
assert run(*sample, 6, 3)[0]
assert run(*sample, 6, 9)[0]
assert not run(*sample, 17, -4)[0]
assert not run(*sample, 6, 10)[0]

print(maxh(*sample))


# puzzle input: target area: x=94..151, y=-156..-103
puzzle = ((94, 151), (-156, -103))
print(maxh(*puzzle))

print(len(findall(*sample)))
print(len(findall(*puzzle)))

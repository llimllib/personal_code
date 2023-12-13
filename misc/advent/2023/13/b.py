import itertools
import sys


def t(g: list[str]) -> list[str]:
    return list("".join(y) for y in zip(*g))


def is_symmetric(g: list[str], i: int) -> bool:
    l = len(g)
    for j in range(1, min(i + 1, l - i - 1)):
        if g[i - j] != g[i + j + 1]:
            return False
    return True


def onediff(a: str, b: str) -> bool:
    return sum(a != b for a, b in zip(a, b)) == 1


def unsmudge(g: list[str]) -> int:
    l = len(g)
    for i, (a, b) in enumerate(itertools.pairwise(g)):
        if a == b or onediff(a, b):
            for j in range(min(i + 1, l - i - 1)):
                if onediff(g[i - j], g[i + j + 1]):
                    h = g[:]
                    h[i - j] = g[i + j + 1]
                    if is_symmetric(h, i):
                        return i + 1
    return 0


print(
    sum(
        unsmudge(grid) * 100 or unsmudge(t(grid))
        for grid in [
            [line.strip() for line in chunk.split("\n")]
            for chunk in sys.stdin.read().strip().split("\n\n")
        ]
    )
)

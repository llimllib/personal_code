import itertools
import sys


def t(g: list[str]) -> list[str]:
    return list("".join(y) for y in zip(*g))


def is_symmetric(g: list[str], i: int) -> int:
    l = len(g)
    for j in range(1, min(i + 1, l - i - 1)):
        if g[i - j] != g[i + j + 1]:
            return 0
    return 1


def symmetry(g: list[str]) -> int:
    return sum(
        i + 1
        for i, (a, b) in enumerate(itertools.pairwise(g))
        if a == b and is_symmetric(g, i)
    )


print(
    sum(
        symmetry(t(grid)) + 100 * symmetry(grid)
        for grid in [
            [line.strip() for line in chunk.split("\n")]
            for chunk in sys.stdin.read().strip().split("\n\n")
            # for chunk in open("input.txt").read().strip().split("\n\n")
        ]
    )
)

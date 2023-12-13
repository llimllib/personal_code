import itertools
import sys

print(
    sum(
        sum(
            i + 1
            for i, (a, b) in enumerate(itertools.pairwise(g))
            if a == b
            and all(
                g[i - j] == g[i + j + 1] for j in range(1, min(i + 1, len(g) - i - 1))
            )
        )
        * 100
        or sum(
            i + 1
            for i, (a, b) in enumerate(itertools.pairwise(gt))
            if a == b
            and all(
                gt[i - j] == gt[i + j + 1]
                for j in range(1, min(i + 1, len(gt) - i - 1))
            )
        )
        for g, gt in (
            (grid, list("".join(y) for y in zip(*grid)))
            for grid in [
                [line.strip() for line in chunk.split("\n")]
                for chunk in sys.stdin.read().strip().split("\n\n")
            ]
        )
    )
)

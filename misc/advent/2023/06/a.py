import itertools
import re
import sys
from math import prod

in1, in2 = itertools.tee(sys.stdin)

# part 1
print(
    prod(
        sum(1 for p in range(t) if (t - p) * p > d)
        for t, d in zip(*[[int(x) for x in re.findall(r"\d+", line)] for line in in1])
    )
)

# part 2
print(
    prod(
        sum(1 for p in range(t) if (t - p) * p > d)
        for t, d in zip(
            *[[int(x) for x in ["".join(re.findall(r"\d+", line))]] for line in in2]
        )
    )
)

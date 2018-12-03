from itertools import *

twos = 0
threes = 0
for line in open("input.txt"):
    counts = [len(list(g)) for k, g in groupby(sorted(line.strip()))]
    print(f"{line} {counts}")
    twos += 1 if 2 in counts else 0
    threes += 1 if 3 in counts else 0
print(f"{twos} {threes} {twos*threes}")

from itertools import combinations
import sys

target = 32321523
lines = [int(l) for l in open("input.txt") if int(l) < target]

for i in range(4,25):
    print(i)
    for j in range(len(lines)-i):
        if sum(lines[j:j+i]) == target:
            r = lines[j:j+i]
            print(min(r), max(r), min(r)+max(r), r)
            sys.exit(0)

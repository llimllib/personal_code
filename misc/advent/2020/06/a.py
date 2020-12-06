import sys

entries = [e.replace("\n", "") for e in open(sys.argv[1]).read().split("\n\n")]
print(sum(len(set(e)) for e in entries))

from functools import reduce

entries = [e.strip() for e in open(sys.argv[1]).read().split("\n\n")]
yeses = []
for e in entries:
    entry = []
    for person in e.split("\n"):
        entry.append(set(person))
    yeses.append(reduce(lambda a, b: a & b, entry, entry[0]))

print(sum(len(y) for y in yeses))

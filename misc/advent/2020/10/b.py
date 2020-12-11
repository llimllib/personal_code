from functools import reduce


def forks(group):
    # forks starts at 1 because we're starting with the first path
    forks = 1
    # index of the pointers on the frontier
    frontier = [0]
    while len(frontier):
        i = frontier.pop(0)
        if i < len(group):
            frontier.insert(0, i + 1)
        # i+1 is always within 3. try 2 and 3
        # print(i, group[i : i + 3], frontier)
        if i < len(group) - 2 and group[i + 2] - group[i] <= 3:
            frontier.append(i + 2)
            forks += 1
        if i < len(group) - 3 and group[i + 3] - group[i] <= 3:
            frontier.append(i + 3)
            forks += 1
    return forks

lines = [int(l) for l in open("a.txt")]
lines = [int(l) for l in open("b.txt")]
lines = [int(l) for l in open("input.txt")]

lines.append(0)

slines = list(sorted(lines))
slines.append(slines[-1] + 3)

j = 0
groups = []
for i in range(len(slines)-1):
    if slines[i]+3 == slines[i+1]:
        groups.append(slines[j:i+1])
        j = i+1

print(slines)
print(groups)
vals = [forks(group) for group in groups]
print(vals, reduce(lambda a,b: a*b, vals, 1))

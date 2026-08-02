import ipdb


def parse(inp):
    ranges, ingredients = inp.split("\n\n")
    ranges = [tuple(map(int, r.strip().split("-"))) for r in ranges.split("\n")]
    ingredients = [int(i) for i in ingredients.strip().split("\n")]
    return ranges, ingredients


def part1(ranges, ingredients):
    total = 0
    for ingredient in ingredients:
        for min, max in ranges:
            # print(ingredient, min, max, min <= ingredient <= max)
            if min <= ingredient <= max:
                total += 1
                break
    return total


ranges, ingredients = parse(open("sample.txt").read())
print(part1(ranges, ingredients))
ranges, ingredients = parse(open("input.txt").read())
print(part1(ranges, ingredients))


def merge(range1, range2):
    min1, max1 = range1
    min2, max2 = range2
    if min2 <= min1 <= max2:
        return [(min(min1, min2), max(max1, max2)), None]
    if min2 <= max1 <= max2:
        return [(min(min1, min2), max(max1, max2)), None]
    if min1 <= min2 <= max1:
        return [(min(min1, min2), max(max1, max2)), None]
    if min1 <= max2 <= max1:
        return [(min(min1, min2), max(max1, max2)), None]
    return (range1, range2)


def findmerge(ranges, r, idx):
    for i, rr in enumerate(ranges):
        if i != idx:
            r1, r2 = merge(r, rr)
            # we found a merge
            if not r2:
                return (i, r1)
    return (None, None)


def part2(ranges):
    found = True
    while found == True:
        found = False
        for i, r in enumerate(ranges):
            to_be_deleted, foundmerge = findmerge(ranges, r, i)
            if foundmerge:
                ranges[i] = foundmerge
                del ranges[to_be_deleted]
                found = True
                break
    return ranges


ranges, _ = parse(open("sample.txt").read())
print(sum([(n2 - n1) + 1 for (n1, n2) in part2(ranges)]))
ranges, _ = parse(open("input.txt").read())
print(sum([(n2 - n1) + 1 for (n1, n2) in part2(ranges)]))

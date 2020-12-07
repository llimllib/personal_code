import re
import sys
import copy

f = sys.argv[1] if len(sys.argv) > 1 else "input.txt"
lines = [e for e in open(f).read().split("\n")]

bags = {}
for bag in lines:
    if not bag:
        continue
    parts = bag.split("contain")
    color = re.findall("(.*?) bags", parts[0])[0]
    insides = []
    if "no other bags" in parts[1]:
        continue
    for contains in parts[1].split(","):
        insides.append(re.findall(r".*?(\d)+ (.*?) bag", contains)[0])
    bags[color] = tuple(insides)

valid_bags = {"shiny gold"}
l = len(valid_bags)
while 1:
    for bag in copy.copy(valid_bags):
        for abag, contains in bags.items():
            # if `abag` is contained within contains, add it to the set of valid bags
            if any(c[1] == bag for c in contains):
                valid_bags.add(abag)

    # if we didn't add any bags, quit
    if len(valid_bags) == l:
        print(valid_bags)
        print(len(valid_bags) - 1)
        sys.exit(0)
    else:
        l = len(valid_bags)

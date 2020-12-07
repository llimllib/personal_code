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


def count(color, bags):
    sum_ = 0
    for n, subcolor in bags[color]:
        n = int(n)
        if subcolor in bags:
            sum_ += n + n * count(subcolor, bags)
        else:
            sum_ += n
    return sum_


print(count("shiny gold", bags))

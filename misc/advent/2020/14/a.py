import re
import sys
import ipdb
from math import *


def parse(lines):
    instructions = []
    for line in lines:
        if line.startswith("mask"):
            instructions.append(("mask", line.split("=")[1].strip()))
            continue
        _, addr, _, val = re.split(r"[\[=\]]", line)
        instructions.append(("set", int(addr), int(val)))

    return instructions


def readmask(mask):
    positive_mask = 0x000000000
    positive_mask = positive_mask | int(mask.replace("X", "0"), 2)

    negative_mask = 0xFFFFFFFFF
    negative_mask = negative_mask ^ int(mask.replace("X", "1"), 2)

    return positive_mask, ~negative_mask


def p1(lines, verbose=False):
    memory = {}
    positive_mask, negative_mask = None, None
    for instr, *args in instructions:
        if instr == "mask":
            positive_mask, negative_mask = readmask(args[0])
            continue
        memory[args[0]] = (args[1] | positive_mask) & negative_mask

    return sum(v for v in memory.values())


# return a list of indexes of every X in s
def findall(s):
    idxs = []
    i = s.find("X")
    while i != -1:
        idxs.append(i)
        i = s.find("X", i + 1)
    return idxs


# given integer value, change the nth bit to `bit`
def nth_bit(val: int, n: int, bit: int):
    if bit:
        return val | 1 << n
    else:
        return val & ~(1 << n)


def p2(lines):
    memory = {}
    mask = None
    for instr, *args in instructions:
        if instr == "mask":
            mask = args[0]
            continue

        addr = args[0] | int(mask.replace("X", "1"), 2)

        xs = findall(mask)
        xl = len(xs)
        for i in range(2 ** xl):
            # the replacement characters. Pad to the right with zeros (ex 1 ->
            # 001), and then convert each number to an integer 0 or 1
            rep = [int(c) for c in bin(i)[2:].rjust(xl, "0")]
            for xi, x in enumerate(xs):
                addr = nth_bit(addr, 35 - x, rep[xi])
            memory[addr] = args[1]
    return sum(v for v in memory.values())


f = sys.argv[1] if len(sys.argv) > 1 else "input.txt"
lines = [l for l in open(f)]
instructions = parse(lines)

print(p1(instructions))
print(p2(instructions))

from collections import defaultdict
from operator import add, sub, lt, le, gt, ge, eq, ne

def go():
    registers = defaultdict(int)
    maxmax = 0
    optable = {"<": lt, "<=": le, ">": gt, ">=": ge, "==": eq, "!=": ne}

    for instruction in open('input.txt'):
        reg, op, val, _, reg2, op2, val2 = instruction.split()
        op = sub if op == "dec" else add
        op2 = optable[op2]

        if op2(registers[reg2], int(val2)):
            registers[reg] = op(registers[reg], int(val))

        maxmax = maxmax if registers[reg] < maxmax else registers[reg]

    print(max(registers.values()))
    print(maxmax)

go()

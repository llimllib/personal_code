from collections import defaultdict
from operator import add, sub

def go():
    registers = defaultdict(int)
    maxmax = 0

    for instruction in open('input.txt'):
        reg, op, val, _, reg2, op2, val2 = instruction.split()
        op = sub if op == "dec" else add

        if eval(f"{registers[reg2]} {op2} {val2}"):
            registers[reg] = op(registers[reg], int(val))

        maxmax = maxmax if registers[reg] < maxmax else registers[reg]

    print(max(registers.values()))
    print(maxmax)

go()

from collections import defaultdict
import sys


def go(inp):
    registers = defaultdict(int)
    registers['a'] = 1
    instructions = list(i.strip() for i in inp)
    ptr = 0
    i = 0
    muls = 0
    while ptr < len(instructions):
        if i % 1000000 == 0:
            pass
            #print(registers)
        inst, a, b = instructions[ptr].split(" ")
        # print(ptr, inst, a, b)
        if instructions[ptr] == "jnz g 2" and registers['d'] * registers['e'] == registers['b']:
            print(registers)
        if inst == "set":
            try:
                registers[a] = int(b)
            except ValueError:
                # this casee is not discussed in the documentation?
                registers[a] = registers[b]
        elif inst == "mul":
            muls += 1
            try:
                registers[a] *= int(b)
            except ValueError:
                registers[a] *= registers[b]
        elif inst == "sub":
            try:
                registers[a] -= int(b)
            except ValueError:
                registers[a] -= registers[b]
        elif inst == "jnz":
            try:
                val = int(a)
            except ValueError:
                val = registers[a]
            if val != 0:
                try:
                    ptr += int(b)
                    continue
                except ValueError:
                    # also not discussed in the docs
                    ptr += registers[b]
                    continue
        else:
            raise TypeError("ohno")
        # print(registers)
        ptr += 1
        i += 1
    print(registers['h'])


if __name__ == "__main__":
    go(open('jimmied.txt'))

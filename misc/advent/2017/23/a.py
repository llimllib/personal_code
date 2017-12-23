from collections import defaultdict
import sys


def go(inp):
    registers = defaultdict(int)
    lastsound = None
    instructions = list(i.strip() for i in inp)
    ptr = 0
    i = 0
    muls = 0
    previnst = None
    loop = 0
    while ptr < len(instructions):
        if i % 1000 == 0:
            sys.stdout.write('.')
            sys.stdout.flush()
        inst, a, b = instructions[ptr].split(" ")
        if inst == previnst:
            loop += 1
            if loop > 20: raise Exception("infinite loop")
        else:
            loop = 0
            previnst = inst
        print(ptr, inst, a, b)
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
        print(registers)
        ptr += 1
        i += 1
    print(muls)


if __name__ == "__main__":
    go(open('input.txt'))

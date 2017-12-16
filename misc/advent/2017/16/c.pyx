def instrs(inp):
    instructions = []
    for instruction in inp.strip().split(","):
        if instruction[0] == "s":
            d = int(instruction[1:])
            instructions.append(("s", d))
        elif instruction[0] == "x":
            a, b = map(int, instruction[1:].split('/'))
            instructions.append(("x", a, b))
        else:
            n1, n2 = map(lambda x: ord(x) - 97, instruction[1:].split('/'))
            instructions.append(("s", n1, n2))
    return instructions

def go(inp, cycles=1):
    cdef int progs[16]
    progs = list(range(n))
    cdef int n = 16
    cdef int d, a, b, n1, n2, i1, i2, i, j
    instructions = instrs()
    for i in range(cycles):
        for inst in instructions:
            if inst[0] == "s":
                d = int(inst[1:])
                for j in range(n-d, 16):
                    progs[(n-d)+j] = progs[j]
            elif inst[0] == "x":
                progs[inst[1]], progs[inst[2]] = progs[inst[2]], progs[inst[1]]
            else:
                i1 = progs.index(inst[1])
                i2 = progs.index(inst[2])
                progs[i1], progs[i2] = progs[i2], progs[i1]
    return ''.join(chr(p + 97) for p in progs)

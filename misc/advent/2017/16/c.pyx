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

def go(inp, int cycles=1):
    cdef char *progs = "abcdefghijklmnop"
    cdef char temp[16] = "abcdefghijklmnop"
    cdef int n = 16
    cdef int d, a, b, n1, n2, i1, i2, i, j
    instructions = instrs(inp)
    for i in range(cycles):
        for inst in instructions:
            print(inst, progs, temp)
            if inst[0] == "s":
                d = inst[1]
                for k in range(16):
                    temp[k] = progs[(16-d+k) % 16]
                for k in range(16):
                    progs[k] = temp[k]
            elif inst[0] == "x":
                print("a", inst, progs, temp)
                temp[0] = progs[inst[1]]
                print("b", inst, progs, temp)
                progs[inst[1]] = progs[inst[2]]
                print("c", inst, progs, temp)
                progs[inst[2]] = temp[0]
            else:
                i1 = progs.index(inst[1])
                i2 = progs.index(inst[2])
                progs[i1], progs[i2] = progs[i2], progs[i1]
    return ''.join(chr(p + 97) for p in progs)

def main():
    print(go(open("input.txt").read()))

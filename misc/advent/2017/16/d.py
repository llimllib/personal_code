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


def go(inp, n=16, cycles=1):
    instructions = instrs(inp)
    progs = list(range(n))
    n = n
    for i in range(cycles):
        for inst in instructions:
            if inst[0] == "s":
                progs = progs[n - inst[1]:] + progs[:n - inst[1]]
            elif inst[0] == "x":
                progs[inst[1]], progs[inst[2]] = progs[inst[2]], progs[inst[1]]
            elif inst[0] == "s":
                i1 = progs.index(inst[1])
                i2 = progs.index(inst[2])
                progs[i1], progs[i2] = progs[i2], progs[i1]
    return ''.join(chr(p + 97) for p in progs)


if __name__ == "__main__":
    print(go("s1,x3/4,pe/b", 5))
    print(go(open("input.txt").read()))
    print(go(open("input.txt").read(), 16, 1000))

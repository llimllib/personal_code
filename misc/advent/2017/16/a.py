def go(inp, n=16, cycles=1):
    progs = list(range(n))
    n = n
    seen = set()
    for i in range(cycles):
        for instruction in inp.strip().split(","):
            if instruction[0] == "s":
                d = int(instruction[1:])
                progs = progs[n - d:] + progs[:n - d]
            elif instruction[0] == "x":
                a, b = map(int, instruction[1:].split('/'))
                progs[a], progs[b] = progs[b], progs[a]
            else:
                n1, n2 = instruction[1:].split('/')
                n1, n2 = map(lambda x: ord(x) - 97, instruction[1:].split('/'))
                i1, i2 = map(lambda x: progs.index(x), (n1, n2))
                progs[i1], progs[i2] = progs[i2], progs[i1]
        progss = ''.join(chr(p + 97) for p in progs)
        if progss in seen:
            print(f'repetition on cycle {i}')
            break
        seen.add(progss)

    return ''.join(chr(p + 97) for p in progs)


if __name__ == "__main__":
    print(go("s1,x3/4,pe/b", 5))
    print(go(open("input.txt").read()))
    print(go(open("input.txt").read(), 16, 1000))

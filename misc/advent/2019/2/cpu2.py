ADD = 1
MUL = 2
QUIT = 99


def cpu(mem, i, j):
    mem = mem[:]

    mem[1] = i
    mem[2] = j

    ip = 0
    while 1:
        op = mem[ip]
        if op == QUIT:
            break

        a, b, out = mem[ip + 1 : ip + 4]
        ip += 4
        if op == ADD:
            mem[out] = mem[a] + mem[b]
            # print("ADD", a, b, out, mem[out])
        elif op == MUL:
            mem[out] = mem[a] * mem[b]
            # print("MUL", a, b, out)
        elif op == QUIT:
            break
        else:
            raise Exception(f"Invalid op: {op}")

    return mem[0]


if __name__ == "__main__":
    mem = [int(n) for n in open("a.txt").read().strip().split(",")]

    for i in range(100):
        for j in range(100):
            out = cpu(mem, i, j)
            if out == 19_690_720:
                raise Exception(f"found it: {i} {j} {100 * i + j}")

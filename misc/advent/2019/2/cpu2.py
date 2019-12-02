ADD = 1
MUL = 2
QUIT = 99


def cpu(mem, i, j):
    mem = mem[:]

    # before running the program, replace position 1 with the value 12 and replace
    # position 2 with the value 2.
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
    # mem = [int(n) for n in "1,1,1,4,99,5,6,0,99".split(",")]
    # mem = [int(n) for n in "2,4,4,5,99,0".split(",")]
    mem = [int(n) for n in open("a.txt").read().strip().split(",")]

    for i in range(100):
        for j in range(100):
            out = cpu(mem, i, j)
            if out == 19_690_720:
                raise Exception(f"found it: {i} {j} {100 * i + j}")

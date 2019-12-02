# mem = [int(n) for n in "1,1,1,4,99,5,6,0,99".split(",")]
# mem = [int(n) for n in "2,4,4,5,99,0".split(",")]
mem = [int(n) for n in open("a.txt").read().strip().split(",")]

# before running the program, replace position 1 with the value 12 and replace
# position 2 with the value 2.
mem[1] = 12
mem[2] = 2

ADD = 1
MUL = 2
QUIT = 99

i = 0
while 1:
    op = mem[i]
    if op == QUIT:
        break

    a, b, out = mem[i + 1 : i + 4]
    i += 4
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

print(mem)

import sys

ADD = 1
MUL = 2
INPUT = 3
OUTPUT = 4
JIT = 5
JIF = 6
LESS = 7
EQ = 8
QUIT = 99

MODE_POS = 0  # position mode
MODE_IMM = 1  # immediate mode

DEBUG = False


def debug(*args):
    if DEBUG:
        print(*args)


def cpu(mem, input_buf):
    ip = 0
    while 1:
        params_and_op = str(mem[ip])

        params = list(int(p) for p in params_and_op[:-2].rjust(3, "0"))
        params.reverse()

        op = int(params_and_op[-2:])
        if op == QUIT:
            break

        if op == ADD:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4
            if params[0] == MODE_POS:
                a = mem[a]
            if params[1] == MODE_POS:
                b = mem[b]
            mem[out] = a + b
            debug(params_and_op, "ADD", a, b, out, mem[out])
        elif op == MUL:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4
            if params[0] == MODE_POS:
                a = mem[a]
            if params[1] == MODE_POS:
                b = mem[b]
            mem[out] = a * b
            debug(params_and_op, "MUL", a, b, out, mem[out])
        elif op == INPUT:
            loc = mem[ip + 1]
            ip += 2
            if params[0] == MODE_IMM:
                raise Exception(f"Unexpected immediate mode input {params_and_op}")
            mem[loc] = int(input_buf.pop(0))
            debug(params_and_op, "INPUT", loc, mem[loc])
        elif op == OUTPUT:
            loc = mem[ip + 1]
            ip += 2
            if params[0] == MODE_POS:
                loc = mem[loc]
            print(loc)
        elif op == JIT:
            test, val = mem[ip + 1 : ip + 3]
            ip += 3
            if params[0] == MODE_POS:
                test = mem[test]
            if params[1] == MODE_POS:
                val = mem[val]
            if test:
                ip = val
            debug(params_and_op, "JIT", test, val, ip)
        elif op == JIF:
            test, val = mem[ip + 1 : ip + 3]
            ip += 3
            if params[0] == MODE_POS:
                test = mem[test]
            if params[1] == MODE_POS:
                val = mem[val]
            if not test:
                ip = val
            debug(params_and_op, "JIF", test, val, ip)
        elif op == LESS:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4
            if params[0] == MODE_POS:
                a = mem[a]
            if params[1] == MODE_POS:
                b = mem[b]
            mem[out] = 1 if a < b else 0
            debug(params_and_op, "LESS", a, b, out, mem[out])
        elif op == EQ:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4
            if params[0] == MODE_POS:
                a = mem[a]
            if params[1] == MODE_POS:
                b = mem[b]
            mem[out] = 1 if a == b else 0
            debug(params_and_op, "EQ", a, b, out, mem[out])
        else:
            raise Exception(f"Invalid op: {op}")


if __name__ == "__main__":
    mem = [int(n) for n in open("in").read().strip().split(",")]

    input_buf = sys.argv[1:]

    cpu(mem, input_buf)

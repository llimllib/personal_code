import sys
import time
import cProfile


def cpu(mem, input_buf, ip=0):
    ADD = "1"
    MUL = "2"
    INPUT = "3"
    OUTPUT = "4"
    JIT = "5"
    JIF = "6"
    LESS = "7"
    EQ = "8"
    QUIT = "99"

    MODE_POS = "0"  # position mode
    MODE_IMM = "1"  # immediate mode

    DEBUG = True

    def debug(*args):
        if DEBUG:
            print(*args)

    ip = ip
    ops = 0
    start = time.time()
    params = ["0", "0", "0"]
    output = ""
    while 1:
        ops += 1
        params_and_op = str(mem[ip])

        l = len(params_and_op)
        i = l - 3
        p = 0
        while i >= 0:
            params[p] = params_and_op[i]
            p += 1
            i -= 1
        while p < 3:
            params[p] = "0"
            p += 1

        op = params_and_op[-2:].lstrip("0")
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
            if DEBUG:
                debug(params_and_op, "ADD", a, b, out, mem[out])
        elif op == MUL:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4
            if params[0] == MODE_POS:
                a = mem[a]
            if params[1] == MODE_POS:
                b = mem[b]
            mem[out] = a * b
            if DEBUG:
                debug(params_and_op, "MUL", a, b, out, mem[out])
        elif op == INPUT:
            loc = mem[ip + 1]
            ip += 2
            if params[0] == MODE_IMM:
                raise Exception(f"Unexpected immediate mode input {params_and_op}")
            mem[loc] = int(input_buf.pop(0))
            if DEBUG:
                debug(params_and_op, "INPUT", loc, mem[loc])
        elif op == OUTPUT:
            loc = mem[ip + 1]
            ip += 2
            if params[0] == MODE_POS:
                loc = mem[loc]
            output += str(loc)
            break
        elif op == JIT:
            test, val = mem[ip + 1 : ip + 3]
            ip += 3
            if params[0] == MODE_POS:
                test = mem[test]
            if params[1] == MODE_POS:
                val = mem[val]
            if test != 0:
                ip = val
            if DEBUG:
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
            if DEBUG:
                debug(params_and_op, "JIF", test, val, ip)
        elif op == LESS:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4
            if params[0] == MODE_POS:
                a = mem[a]
            if params[1] == MODE_POS:
                b = mem[b]
            mem[out] = 1 if a < b else 0
            if DEBUG:
                debug(params_and_op, "LESS", a, b, out, mem[out])
        elif op == EQ:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4
            if params[0] == MODE_POS:
                a = mem[a]
            if params[1] == MODE_POS:
                b = mem[b]
            mem[out] = 1 if a == b else 0
            if DEBUG:
                debug(params_and_op, "EQ", a, b, out, mem[out])
        else:
            raise Exception(f"Invalid op: {op}")

    dur = time.time() - start
    return mem, output, ip

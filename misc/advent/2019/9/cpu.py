import sys
import time

BUF_SIZE = 1000000


def cpu(mem, input_buf):
    ADD = "1"
    MUL = "2"
    INPUT = "3"
    OUTPUT = "4"
    JIT = "5"
    JIF = "6"
    LESS = "7"
    EQ = "8"
    ADJ_REL_BASE = "9"
    QUIT = "99"

    MODE_POS = "0"  # position mode
    MODE_IMM = "1"  # immediate mode
    MODE_REL = "2"  # relative mode

    DEBUG = False

    def debug(*args):
        if DEBUG:
            print(*args)

    ip = 0
    ops = 0
    start = time.time()
    modes = ["0", "0", "0"]
    output = ""
    relative_base = 0

    def aparam(val, mode, mem):
        if mode == MODE_POS:
            return mem[val]
        if mode == MODE_REL:
            return mem[relative_base + val]
        return val

    def outparam(val, mode):
        if mode == MODE_POS:
            return val
        if mode == MODE_REL:
            return relative_base + val
        # Parameters that an instruction writes to will never be in immediate mode.
        # https://adventofcode.com/2019/day/5#part2
        raise Exception("Immediate mode output parameters are invalid")

    while 1:
        ops += 1
        modes_and_op = str(mem[ip])

        l = len(modes_and_op)
        i = l - 3
        p = 0
        while i >= 0:
            modes[p] = modes_and_op[i]
            p += 1
            i -= 1
        while p < 3:
            modes[p] = "0"
            p += 1

        op = modes_and_op[-2:].lstrip("0")
        if op == QUIT:
            break

        if op == ADD:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4

            a = aparam(a, modes[0], mem)
            b = aparam(b, modes[1], mem)
            out = outparam(out, modes[2])

            mem[out] = a + b
            if DEBUG:
                debug(modes_and_op, "ADD", a, b, out, mem[out])
        elif op == MUL:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4

            a = aparam(a, modes[0], mem)
            b = aparam(b, modes[1], mem)
            out = outparam(out, modes[2])

            mem[out] = a * b
            if DEBUG:
                debug(modes_and_op, "MUL", a, b, out, mem[out])
        elif op == INPUT:
            loc = mem[ip + 1]
            ip += 2

            loc = outparam(loc, modes[0])

            mem[loc] = int(input_buf.pop(0))
            if DEBUG:
                debug(modes_and_op, "INPUT", loc, mem[loc])
        elif op == OUTPUT:
            loc = mem[ip + 1]
            ip += 2

            loc = aparam(loc, modes[0], mem)

            output += str(loc) + "\n"
            if DEBUG:
                debug(modes_and_op, "OUTPUT", loc)
        elif op == JIT:
            test, val = mem[ip + 1 : ip + 3]
            ip += 3

            test = aparam(test, modes[0], mem)
            val = aparam(val, modes[1], mem)

            if test:
                ip = val
            if DEBUG:
                debug(modes_and_op, "JIT", test, val, ip)
        elif op == JIF:
            test, val = mem[ip + 1 : ip + 3]
            ip += 3

            test = aparam(test, modes[0], mem)
            val = aparam(val, modes[1], mem)

            if not test:
                ip = val
            if DEBUG:
                debug(modes_and_op, "JIF", test, val, ip)
        elif op == LESS:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4

            a = aparam(a, modes[0], mem)
            b = aparam(b, modes[1], mem)
            out = outparam(out, modes[2])

            mem[out] = 1 if a < b else 0
            if DEBUG:
                debug(modes_and_op, "LESS", a, b, out, mem[out])
        elif op == EQ:
            a, b, out = mem[ip + 1 : ip + 4]
            ip += 4

            a = aparam(a, modes[0], mem)
            b = aparam(b, modes[1], mem)
            out = outparam(out, modes[2])

            mem[out] = 1 if a == b else 0
            if DEBUG:
                debug(modes_and_op, "EQ", a, b, out, mem[out])
        elif op == ADJ_REL_BASE:
            adj = mem[ip + 1]
            ip += 2

            adj = aparam(adj, modes[0], mem)

            relative_base += adj
            if DEBUG:
                debug(modes_and_op, "ADJ_REL_BASE", adj, relative_base)
        else:
            raise Exception(f"Invalid op: {op}")

    dur = time.time() - start
    sys.stderr.write(f"{output}\n")
    sys.stderr.write(f"run took {dur}s for {ops} ops [{ops / dur:,.0f} ops/s]\n")


if __name__ == "__main__":
    input_file = sys.argv[1]
    input_buf = sys.argv[2:]

    mem = [int(n) for n in open(input_file).read().strip().split(",")]

    mem += [""] * (BUF_SIZE - len(mem))

    cpu(mem, input_buf)

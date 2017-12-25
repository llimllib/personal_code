from collections import defaultdict


def go(inp, iters):
    machine = {}
    for line in inp:
        state, val, towrite, move, newstate = line.strip().split()
        machine[(state, int(val))] = (int(towrite), move, newstate)

    tape = defaultdict(int)
    ptr = 0
    state = "A"
    for _ in range(iters):
        towrite, move, state = machine[(state, tape[ptr])]
        tape[ptr] = towrite
        ptr = ptr + 1 if move == "R" else ptr - 1

    print(sum(tape.values()), state, ptr)


if __name__ == "__main__":
    # go(open("sample.tt"), 6)
    go(open("input2.txt"), 12134527)

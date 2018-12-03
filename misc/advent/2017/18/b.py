from collections import defaultdict
from queue import Queue, Empty
import sys


def start_process(id_, instructions, inq, outq):
    registers = defaultdict(int)
    registers["p"] = id_

    i = 0
    sends = 0
    while i < len(instructions):
        inst, *args = instructions[i].split(" ")

        # This seems to be required, though the documentation is very sketchy on it
        if len(args) > 1:
            args[1] = registers[args[1]] if args[1].isalpha() else int(args[1])

        #print(id_, i, inst, args)
        if inst == "set":
            registers[args[0]] = args[1]
        elif inst == "mul":
            registers[args[0]] *= args[1]
        elif inst == "add":
            registers[args[0]] += args[1]
        elif inst == "mod":
            registers[args[0]] %= args[1]
        elif inst == "snd":
            sends += 1
            #print(f"{id_} sends: {sends} {registers[args[0]]}")
            outq.put(registers[args[0]])
        elif inst == "rcv":
            while 1:
                try:
                    registers[args[0]] = inq.get_nowait()
                    break
                except Empty:
                    yield (False, sends)
        elif inst == "jgz":
            value = registers[args[0]] if args[0].isalpha() else int(args[0])
            if value > 0:
                i += args[1]
                yield (True, sends)
                continue
        else:
            raise TypeError("ohno")
        #print(id_, registers)
        i += 1
        yield (True, sends)


def go(inp):
    q0 = Queue()
    q1 = Queue()
    proc1 = start_process(0, inp, q0, q1)
    proc2 = start_process(1, inp, q1, q0)
    a = b = True
    while a != False or b != False or sends > 150000:
        a, _ = next(proc1)
        b, sends = next(proc2)
    print("sends: ", sends)


if __name__ == "__main__":
    #go(['snd 1', 'snd 2', 'snd p', 'rcv a', 'rcv b', 'rcv c', 'rcv d'])
    go(list(i.strip() for i in open("input.txt")))

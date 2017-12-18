from collections import defaultdict


def go(inp):
    registers = defaultdict(int)
    lastsound = None
    instructions = list(i.strip() for i in inp)
    i = 0
    while i < len(instructions):
        inst, *args = instructions[i].split(" ")
        print(i, inst, args)
        if inst == "set":
            try:
                registers[args[0]] = int(args[1])
            except ValueError:
                # this casee is not discussed in the documentation?
                registers[args[0]] = registers[args[1]]
        elif inst == "mul":
            try:
                registers[args[0]] *= int(args[1])
            except ValueError:
                registers[args[0]] *= registers[args[1]]
        elif inst == "add":
            try:
                registers[args[0]] += int(args[1])
            except ValueError:
                registers[args[0]] *= registers[args[1]]
        elif inst == "mod":
            try:
                registers[args[0]] %= int(args[1])
            except ValueError:
                registers[args[0]] %= registers[args[1]]
        elif inst == "snd":
            lastsound = registers[args[0]]
            print(f"setting sound {lastsound}")
        elif inst == "rcv":
            if registers[args[0]] != 0:
                return lastsound
        elif inst == "jgz":
            if registers[args[0]] > 0:
                try:
                    i += int(args[1])
                    continue
                except ValueError:
                    # also not discussed in the docs
                    i += registers[args[1]]
                    continue
        else:
            raise TypeError("ohno")
        print(registers)
        i += 1


if __name__ == "__main__":
    print(
        go("""set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2""".split("\n")))
    print(go(open("input.txt")))

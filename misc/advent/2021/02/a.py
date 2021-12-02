from utils import opensplit


def pos(cmds):
    hpos, depth = 0, 0
    for cmd, n in cmds:
        n = int(n)
        if cmd == "forward":
            hpos += n
        if cmd == "down":
            depth += n
        if cmd == "up":
            depth -= n
    return hpos, depth, hpos * depth


print(pos(opensplit("small.txt")))
print(pos(opensplit("input.txt")))


def aim(cmds):
    aim, hpos, depth = 0, 0, 0
    for cmd, n in cmds:
        n = int(n)
        if cmd == "forward":
            hpos += n
            depth += aim * n
        if cmd == "down":
            aim += n
        if cmd == "up":
            aim -= n
    return hpos, depth, hpos * depth


print(aim(opensplit("small.txt")))
print(aim(opensplit("input.txt")))

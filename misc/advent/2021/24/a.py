from typing import List, Any, Dict

def tryint(x):
    try:
        return int(x)
    except ValueError:
        return x


def parse(it):
    cmds = []
    for line in it:
        if not line.strip():
            continue
        cmds.append(list(map(tryint, line.strip().split(" "))))
    return cmds

def exec_(cmds, inputs: List[int], debug=False):
    regs = {
        'w': 0,
        'x': 0,
        'y': 0,
        'z': 0,
    }
    for cmd in cmds:
        if debug:
            print('---')
            print(inputs)
            print(" ".join(map(str, cmd)))
        if len(cmd) == 3 and isinstance(cmd[-1], str):
            cmd[-1] = regs[cmd[-1]]
        match cmd:
            case ['inp', x]:
                regs[x] = inputs.pop()
            case ['add', x, y]:
                regs[x] = regs[x] + y
            case ['mul', x, y]:
                regs[x] = regs[x] * y
            case ['div', x, y]:
                regs[x] = regs[x] // y
            case ['mod', x, y]:
                regs[x] = regs[x] % y
            case ['eql', x, y]:
                regs[x] = 1 if regs[x] == y else 0
            case _:
                raise Exception(f"Failed to parse {cmd}")
        if debug:
            print(regs)
    return regs


def search():
    prog = parse(open("input.txt"))
    for i in range(99_999_999_999_999 + 1, 11_111_111_111_111, -1):
        s = str(i)
        if not '0' in s:
            regs = exec_(prog, list(reversed(list(map(int, s)))))
            if not regs['z']:
                print(s, regs)
                return

assert "".join(map(str, exec_(parse(open("small.txt")), [13]).values())) == "1101"
search()

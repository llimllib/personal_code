def steps(x, y):
    x = abs(x)
    y = abs(y)
    m = min(x, y)
    return m + (y - m) / 2


def go(path):
    x = 0
    y = 0
    maxsteps = 0
    for step in path:
        if step == "n": y += 2
        if step == "s": y -= 2
        if step == "ne":
            y += 1
            x += 1
        if step == "se":
            y -= 1
            x += 1
        if step == "nw":
            y += 1
            x -= 1
        if step == "sw":
            y -= 1
            x -= 1
        s = steps(x, y)
        maxsteps = max(maxsteps, s)

    print(maxsteps, steps(x, y))


if __name__ == "__main__":
    go(open("input.txt").read().strip().split(","))
    print("---")
    go("ne,ne,ne")  # 3
    print("---")
    go("ne,ne,sw,sw")  # 0
    print("---")
    go("ne,ne,s,s")  # 2
    print("---")
    go("se,sw,se,sw,sw")  # 3

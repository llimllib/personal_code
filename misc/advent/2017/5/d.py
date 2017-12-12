text = open("input.txt").read().strip().split("\n")
cmds = [int(cmd) for cmd in text]

location = 0
counter = 0
g = cmds.__getitem__
s = cmds.__setitem__
while 1:
    try:
        cmd = g(location)
        if cmd >= 3:
            s(location, cmd - 1)
        else:
            s(location, cmd + 1)
        location += cmd
        counter += 1
        if location < 0:
            print(counter)
            break
    except:
        print(counter+1)
        break

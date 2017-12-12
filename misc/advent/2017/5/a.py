text = open("input.txt").read().strip().split("\n")
cmds = [int(cmd) for cmd in text]

location = 0
counter = 0
l = len(cmds)
while 1:
    cmd = cmds[location]
    if cmd >= 3:
        cmds[location] -= 1
    else:
        cmds[location] += 1
    location += cmd
    if location < 0 or location > l-1:
        print(counter+1)
        break
    counter += 1

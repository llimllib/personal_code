# modified for line_profiler
@profile
def run(text, cmds):
    location = 0
    counter = 0
    l = len(cmds)
    while 1:
        try:
            cmd = cmds[location]
            if cmd >= 3:
                cmds[location] -= 1
            else:
                cmds[location] += 1
            location += cmd
            counter += 1
        except:
            print(counter+1)
            break

if __name__=="__main__":
    text = open("input.txt").read().strip().split("\n")
    cmds = [int(cmd) for cmd in text]

    run(text, cmds)

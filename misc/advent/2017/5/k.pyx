import numpy as np
cimport numpy as np

def run(np.ndarray cmds):
    cdef int location, counter, l
    location = 0
    counter = 0
    l = 1003
    while 1:
        try:
            cmd = cmds[location]
            if cmd >= 3:
                cmds[location] -= 1
            else:
                cmds[location] += 1
            location += cmd
            if location < 0:
                print(counter)
                break
            counter += 1
        except:
            print(counter)
            break

cmds = np.array([int(cmd) for cmd in open("input.txt")])
run(cmds)


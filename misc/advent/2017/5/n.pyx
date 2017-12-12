def run():
    cdef int[1003] cmds = [int(n) for n in open("input.txt")]

    cdef int location = 0
    cdef int counter = 0
    cdef int l = len(cmds)
    cdef int cmd = 0
    while 1:
        cmd = cmds[location]
        if cmd >= 3:
            cmds[location] -= 1
        else:
            cmds[location] += 1
        location += cmd
        if location < 0 or location > l:
            print(counter)
            break
        counter += 1

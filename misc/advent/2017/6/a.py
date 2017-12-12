def run():
    # part 1
    txt = open("input.txt").read().split("\t")
    mems = map(int, txt)
    # part 2
    mems = [0, 1, 14, 14, 13, 12, 11, 9, 9, 8, 7, 6, 5, 3, 2, 4]
    states = set()
    l = len(mems)
    cycles = 0

    while tuple(mems) not in states:
        states.add(tuple(mems))
        max_ = 0
        ptr = -1
        for i, mem in enumerate(mems):
            if mem > max_:
                max_ = mem
                ptr = i
        mems[ptr] = 0
        for j in range(max_):
            mems[(ptr+j+1)%l] += 1
        cycles += 1
    print(cycles)

if __name__=="__main__":
    run()

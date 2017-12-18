def go(inp, iters=2018):
    buf = [0]
    pos = 0
    ptr = 0
    for i in range(1, iters):
        #for i in range(1, 10):
        ptr = (ptr + inp) % len(buf)
        buf.insert(ptr + 1, i)
        #print(buf)
        ptr += 1
    idx = buf.index(2017)
    print(buf[idx - 5:idx + 5])


if __name__ == "__main__":
    #go(3)
    go(312)
    go(312, 50000000)

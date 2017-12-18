cdef run(int mod, int iters):
    buf = [0] * 50000000
    cdef int pos = 0
    cdef int ptr = 0
    for i in range(1, iters+1):
        if (i % 1000000) == 0:
            print(i)
        ptr = (ptr + mod) % (i)
        buf.insert(ptr + 1, i)
        #print(buf)
        ptr += 1
    idx = buf.index(2017)
    print(buf[idx - 5:idx + 5])


def go():
    run(3, 2017)
    run(312, 2017)
    run(312, 50000000)

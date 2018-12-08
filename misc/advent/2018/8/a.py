def read_kids(buf, nkids):
    n = 0
    for i in range(nkids):
        nkids, nmeta, buf = buf[0], buf[1], buf[2:]
        # print(f"{nkids}, {nmeta}, {n}, {buf}")
        if nkids > 0:
            buf, nn = read_kids(buf, nkids)
            n += nn
        meta, buf = buf[:nmeta], buf[nmeta:]
        n += sum(meta)
    return buf, n


def do(infile):
    buf = [int(n) for n in infile.read().strip().split()]
    nkids, nmeta = buf[0], buf[1]
    buf, n = read_kids(buf[2:], nkids)
    assert len(buf) == nmeta, f"{buf} {nmeta}"
    print(n + sum(buf))


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))

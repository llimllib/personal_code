def go(basea, baseb, iters):
    basea = basea
    baseb = baseb
    judge = 0
    for i in range(iters):
        basea = basea * 16807 % 2147483647
        baseb = baseb * 48271 % 2147483647
        if basea & 0xFFFF == baseb & 0xFFFF:
            judge += 1
    return judge


def gen_a(a):
    a = a
    while 1:
        a = a * 16807 % 2147483647
        if a % 4 == 0:
            yield a


def gen_b(b):
    b = b
    while 1:
        b = b * 48271 % 2147483647
        if b % 8 == 0:
            yield b


def go2(basea, baseb, iters):
    gena = gen_a(basea)
    genb = gen_b(baseb)
    judge = 0
    for i in range(iters):
        a = next(gena)
        b = next(genb)
        if a & 0xFFFF == b & 0xFFFF:
            judge += 1
    return judge


def go3(basea, baseb, iters):
    a = basea
    b = baseb
    judge = 0
    for i in range(iters):
        while 1:
            a = a * 16807 % 2147483647
            if a % 4 == 0: break
        while 1:
            b = b * 48271 % 2147483647
            if b % 8 == 0: break
        if a & 0xFFFF == b & 0xFFFF:
            judge += 1
    return judge


if __name__ == "__main__":
    #print(go(783, 325, 40000000))
    # generator style runs in pypy in .74s. python is ~16s
    #print(go2(783, 325, 5000000))
    # straight iterative runs in .42s
    print(go3(783, 325, 5000000))

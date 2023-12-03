from ipdb import set_trace as t


def run_reddit():
    xs = [*enumerate(int(x) * 811589153 for x in open("input.txt"))]
    for x in xs * 10:
        xs.pop(j := xs.index(x))
        xs.insert((j + x[1]) % len(xs), x)
        yield ([x[1] for x in xs])
    xs = [x for _, x in xs]
    print(sum(xs[(xs.index(0) + 1000 * p) % len(xs)] for p in [1, 2, 3]))


def index_unset(data, n, ixs):
    ix = -1
    while 1:
        ix = data.index(n, ix + 1)
        if not ixs[ix]:
            return ix

    raise AssertionError("unreachable")


def mix(data, comp):
    sl = len(data)
    orig = data[:]
    for round in range(10):
        # bit mask tracking what values have been popped
        ixs = [0 for _ in data]
        for n in orig:
            if n == 1797669973895:
                t()
            ix = index_unset(data, n, ixs)
            x = data.pop(ix)
            ixs.pop(ix)
            newix = (x + ix) % (sl - 1)
            data.insert(newix, x)
            ixs.insert(newix, 1)
            res = next(comp)
            if res != data:
                t()
                print(round, n)

    ix = data.index(0)
    return data[(ix + 1000) % sl] + data[(ix + 2000) % sl] + data[(ix + 3000) % sl]


r = run_reddit()
key = 811589153
mix([int(n) * key for n in open("input.txt")], r)

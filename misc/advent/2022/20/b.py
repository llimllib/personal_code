from ipdb import set_trace as t


def index_unset(data, n, ixs):
    ix = -1
    while 1:
        ix = data.index(n, ix + 1)
        if not ixs[ix]:
            return ix

    raise AssertionError("unreachable")


# an attempt to make list splicing work, get the same exact failing answer
def mix(data, rounds=1, verbose=False):
    sl = len(data)
    orig = data[:]
    for _ in range(rounds):
        # bit mask tracking what values have been popped
        ixs = [0 for _ in data]
        for n in orig:
            if verbose:
                print(data, ixs)
            ix = index_unset(data, n, ixs)
            newix = (data[ix] + ix) % (sl - 1)
            if ix < newix:
                # rotate first element forward
                data[ix : newix + 1] = data[ix + 1 : newix + 1] + [data[ix]]
                ixs[ix : newix + 1] = ixs[ix + 1 : newix + 1] + [1]
            else:
                # backward
                data[newix : ix + 1] = [data[ix]] + data[newix:ix]
                ixs[newix : ix + 1] = [1] + ixs[newix:ix]

    ix = data.index(0)
    return data[(ix + 1000) % sl] + data[(ix + 2000) % sl] + data[(ix + 3000) % sl]


sample = [1, 2, -3, 3, -2, 0, 4]
res = mix(sample[:], 1, True)
assert res == 3, res

data = [int(n) for n in open("input.txt").read().strip().split("\n")]
res = mix(data[:])
print("part 1:", res)
assert res == 14526

key = 811589153
res = mix([s * key for s in sample], 10, True)
assert res == 1623178306

# -5070809027944 is not the right answer
print("part 2:", mix([d * key for d in data], 10, False))

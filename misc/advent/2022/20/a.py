from ipdb import set_trace as t


def mix(data):
    sl = len(data)
    # bit mask tracking what values have been popped
    ixs = [0 for _ in data]
    for i in range(sl):
        ix = ixs.index(0)
        x = data.pop(ix)
        ixs.pop(ix)
        # print("moving", x, "from", ix, "to", (x + ix) % sl)
        newix = (x + ix) % (sl - 1)
        data.insert(newix, x)
        ixs.insert(newix, 1)
        ixs[newix] = 1
        # print(data)
        # print(ixs)

    ix = data.index(0)
    return data[(ix + 1000) % sl] + data[(ix + 2000) % sl] + data[(ix + 3000) % sl]


res = mix([1, 2, -3, 3, -2, 0, 4])
assert res == 3, res

# -8224 and -2392 are both not right
data = [int(n) for n in open("input.txt").read().strip().split("\n")]
print(mix(data))

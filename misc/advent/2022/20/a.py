from ipdb import set_trace as t


def mix(data):
    sl = len(data)
    ixs = []
    for i in range(sl):
        ix = len([ix for ix in ixs if ix < i]) % sl
        x = data.pop(ix)
        # print("moving", x, "from", ix, "to", (x + ix) % sl)
        newix = (x + ix) % (sl - 1)
        data.insert(newix, x)
        ixs.append(newix)
        # print(data)

    ix = data.index(0)
    return data[(ix + 1000) % sl] + data[(ix + 2000) % sl] + data[(ix + 3000) % sl]


res = mix([1, 2, -3, 3, -2, 0, 4])
assert res == 3, res

# -8224 and -2392 are both not right
data = [int(n) for n in open("input.txt").read().strip().split("\n")]
print(mix(data))

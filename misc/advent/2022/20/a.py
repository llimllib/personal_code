def mix(data, rounds=1, verbose=False):
    sl = len(data)
    for _ in range(rounds):
        # bit mask tracking what values have been popped
        ixs = [0 for _ in data]
        for _ in range(sl):
            if verbose:
                print(data)
            ix = ixs.index(0)
            x = data.pop(ix)
            ixs.pop(ix)
            print(x, x % sl)
            newix = ((x % sl) + ix) % (sl - 1)
            data.insert(newix, x)
            ixs.insert(newix, 1)
            ixs[newix] = 1

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

print(mix([d * key for d in data], 10))

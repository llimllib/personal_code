def mix(data, rounds=1):
    sl = len(data)
    for _ in range(rounds):
        # bit mask tracking what values have been popped
        ixs = [0 for _ in data]
        for _ in range(sl):
            ix = ixs.index(0)
            x = data.pop(ix)
            ixs.pop(ix)
            newix = (x + ix) % (sl - 1)
            data.insert(newix, x)
            ixs.insert(newix, 1)
            ixs[newix] = 1

    ix = data.index(0)
    return data[(ix + 1000) % sl] + data[(ix + 2000) % sl] + data[(ix + 3000) % sl]


sample = [1, 2, -3, 3, -2, 0, 4]
res = mix(sample)
assert res == 3, res

# 14526 is the right answer
data = [int(n) for n in open("input.txt").read().strip().split("\n")]
print(mix(data))

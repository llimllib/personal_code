def solve(busids, busidxs, minans):
    i = ((minans // busids[0]) * busids[0]) - busidxs[0]
    p = i
    while 1:
        for idx, busid in enumerate(busids[1:]):
            if (i - (busid - busidxs[idx + 1])) % busid != 0:
                break
        else:
            print(i)
            break
        i += busids[0]
        if i - p > 10000000000:
            print(i)
            p = i


# solve([5, 11, 13], [0, 1, 2], 100)
# solve([1789, 37, 47, 1889], [0, 1, 2, 3], 100)
# solve([67, 7, 59, 61], [0, 2, 3, 4], 100)
# solve([67, 7, 59, 61], [0, 1, 3, 4], 100)
# solve([67, 7, 59, 61], [0, 1, 2, 3], 100)
# solve([7, 13, 59, 31, 19], [0, 1, 4, 6, 7], 100)
# solve([59, 7, 13, 31, 19], [4, 0, 1, 6, 7], 100)
solve(
    [883, 19, 37, 23, 13, 17, 797, 41, 29],
    [19, 0, 13, 27, 32, 36, 50, 60, 79],
    100000000000000,
)
# 100,000,000,000,000

def do(fishes, days=80):
    fish = {}
    for i in range(9):
        fish[i] = 0

    for d in map(int, fishes.split(",")):
        fish[d] += 1

    for _ in range(days):
        parents = fish[0]
        for i in range(8):
            fish[i] = fish[i + 1]
        # every fish has a child
        fish[8] = parents
        # and becomes a 6
        fish[6] += parents

    return sum(fish[i] for i in range(9))


print(do("3,4,3,1,2"))
print(do(open("input.txt").read()))
print(do("3,4,3,1,2", 256))
print(do(open("input.txt").read(), 256))

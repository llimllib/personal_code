from itertools import tee

def skip(it):
    next(it)
    return it

def iterpair(it):
    it1, it2 = tee(it)
    return zip(it1, skip(it2))

def itertrip(it):
    it1, it2, it3 = tee(it, 3)
    return zip(it1, skip(it2), skip(skip(it3)))

def n_increase(it):
    return sum(a < b for a, b in it)

print(
    n_increase(
        iterpair(map(int, open('sample.txt'))),
    )
)
print(
    n_increase(
        iterpair(map(int, open('input.txt'))),
    )
)

print(
    n_increase(
        iterpair(
            map(sum, itertrip(map(int, open('sample.txt'))))
        )
    )
)
print(
    n_increase(
        iterpair(
            map(sum, itertrip(map(int, open('input.txt'))))
        )
    )
)

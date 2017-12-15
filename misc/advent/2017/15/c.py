from numba import jit


# .98s. Three timmes slower than pypy, twice as slow as cython,
# but 20x faster than raw cpython
@jit
def go(basea, baseb, iters):
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
    print(go(783, 325, 5000000))

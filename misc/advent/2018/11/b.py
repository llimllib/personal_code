import math
import numpy as np


def maxn(a, n):
    return max((np.sum(a[i:i + n, j:j + n]), i, j)
               for i in range(300 - n) for j in range(300 - n))


def f(x, y, serial):
    power = ((x + 10) * y + serial) * (x + 10)
    return math.floor((power % 1000) / 100) - 5


def do(serial):
    m = np.zeros((300, 300))
    z = np.array([f(x, y, serial) for (x, y), _ in np.ndenumerate(m)]).reshape(
        300, 300)
    (msum, x, y), n = max((maxn(z, n), n) for n in range(1, 300))
    print(f"{x}, {y}, {n}: {msum}")
    return z


if __name__ == "__main__":
    do(18)
    do(42)
    do(9424)

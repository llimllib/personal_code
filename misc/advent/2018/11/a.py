import math
import numpy as np


def max3x3(a):
    return max((np.sum(a[i:i + 3, j:j + 3]), i, j)
               for i in range(297) for j in range(297))


def f(x, y, serial):
    power = ((x + 10) * y + serial) * (x + 10)
    return math.floor((power % 1000) / 100) - 5


def do(serial):
    m = np.zeros((300, 300))
    z = np.array([f(x, y, serial) for (x, y), _ in np.ndenumerate(m)]).reshape(
        300, 300)
    msum, x, y = max3x3(z)
    print(f"{x}, {y}: {msum}")
    return z


if __name__ == "__main__":
    do(18)
    do(42)
    do(9424)

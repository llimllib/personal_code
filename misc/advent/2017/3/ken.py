from math import floor, sqrt

number = 347991


def get_coords(n):
    '''Closed form solution.

    Taken from here: https://danpearcymaths.wordpress.com/2012/09/30/infinity-programming-in-geogebra-and-failing-miserably/
    '''

    p = floor(sqrt(4*n + 1))
    q = n - floor(p ** 2 / 4)
    z = q * 1j ** p + (floor((p + 2) / 4) - 1j * floor((p + 1) / 4)) * 1j ** (p - 1)
    return int(z.real), int(z.imag)

print(sum(map(abs, get_coords(number))))


# Iteratively calculate each sum. Assumes the answer can be found in a
# 11x11 grid, which turns out to be the case
size_x, size_y = 11, 11
grid = [[0] * size_x for row in range(size_y)]

x = 5
y = 5

grid[y][x] = 1

for num in range(size_x * size_y):
    coords = get_coords(num)
    current_x, current_y = x + coords[0], y - coords[1]

    total = \
        grid[current_y - 1][current_x - 1] + \
        grid[current_y - 1][current_x    ] + \
        grid[current_y - 1][current_x + 1] + \
        grid[current_y    ][current_x - 1] + \
        grid[current_y    ][current_x    ] + \
        grid[current_y    ][current_x + 1] + \
        grid[current_y + 1][current_x - 1] + \
        grid[current_y + 1][current_x    ] + \
        grid[current_y + 1][current_x + 1]

    grid[current_y][current_x] = total

    if total > number:
        print(total)
        break

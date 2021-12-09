from functools import reduce
from operator import mul
from itertools import islice


def take(n, iterable):
    "Return first n items of the iterable as a list"
    return list(islice(iterable, n))


def parse(it):
    matrix = []
    for line in it:
        if line.strip():
            matrix.append(list(map(int, list(line.strip()))))
    return matrix


def get(matrix, x, y, default):
    if x < 0 or y < 0:
        return default
    try:
        return matrix[x][y]
    except IndexError:
        return default


def basin(matrix, r, c):
    maxrow = len(matrix) - 1
    maxcol = len(matrix[0]) - 1

    frontier = set([(r, c)])
    seen = []
    size = 0
    while len(frontier):
        r, c = frontier.pop()
        seen.append((r, c))
        if matrix[r][c] != 9:
            size += 1
            if r > 0 and (r - 1, c) not in seen:
                frontier.add((r - 1, c))
            if c > 0 and (r, c - 1) not in seen:
                frontier.add((r, c - 1))
            if r < maxrow and (r + 1, c) not in seen:
                frontier.add((r + 1, c))
            if c < maxcol and (r, c + 1) not in seen:
                frontier.add((r, c + 1))

    return size


def mins(matrix):
    mins = 0
    sum_ = 0
    basins = []
    for r in range(len(matrix)):
        for c in range(len(matrix[0])):

            if all(
                matrix[r][c] < x
                for x in (
                    get(matrix, r - 1, c, 99),
                    get(matrix, r, c - 1, 99),
                    get(matrix, r + 1, c, 99),
                    get(matrix, r, c + 1, 99),
                )
            ):
                mins += 1
                sum_ += matrix[r][c] + 1
                basins.append(basin(matrix, r, c))
    return mins, sum_, reduce(mul, take(3, sorted(basins, reverse=True)))


print(mins(parse(open("small.txt"))))
print(mins(parse(open("input.txt"))))

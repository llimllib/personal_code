import re
from knothash import hash


def fill(grid, x, y, region):
    grid[x][y] = region
    if x > 0 and grid[x - 1][y] == "1":
        fill(grid, x - 1, y, region)
    if x < len(grid) - 1 and grid[x + 1][y] == "1":
        fill(grid, x + 1, y, region)
    if y > 0 and grid[x][y - 1] == "1":
        fill(grid, x, y - 1, region)
    if y < len(grid) - 1 and grid[x][y + 1] == "1":
        fill(grid, x, y + 1, region)


def count_regions(grid):
    region = 2
    for i, row in enumerate(grid):
        for j, c in enumerate(row):
            if c == "1":
                fill(grid, i, j, str(region))
                region += 1
    return region - 2


def go(inp):
    rows = []
    for i in range(128):
        rows.append(hash(f"{inp}-{i}"))
    grid = []
    n = 0
    for row in rows:
        binary = f"{int(row,16):0128b}"
        grid.append(list(binary))
        n += len(re.findall("1", binary))
    print(n)

    print(count_regions(grid))


if __name__ == "__main__":
    inp = "flqrgnkx"
    inp = "uugsqrei"
    go(inp)

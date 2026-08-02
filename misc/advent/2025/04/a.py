import sys


def mapprint(map):
    for row in map:
        for col in row:
            sys.stdout.write(col)
        print()


def part1(map):
    total = 0
    height = len(map) - 1
    width = len(map[0]) - 1
    for x, row in enumerate(map):
        for y, _ in enumerate(row):
            neighbors = 0
            if map[x][y] == "@":
                if x > 0 and y > 0 and map[x - 1][y - 1] == "@":
                    neighbors += 1
                if x > 0 and map[x - 1][y] == "@":
                    neighbors += 1
                if x > 0 and y < height and map[x - 1][y + 1] == "@":
                    neighbors += 1
                if y < height and map[x][y + 1] == "@":
                    neighbors += 1
                if x < width and y < height and map[x + 1][y + 1] == "@":
                    neighbors += 1
                if x < width and map[x + 1][y] == "@":
                    neighbors += 1
                if x < width and y > 0 and map[x + 1][y - 1] == "@":
                    neighbors += 1
                if y > 0 and map[x][y - 1] == "@":
                    neighbors += 1
                if neighbors < 4:
                    total += 1
    return total


sample = list(map(list, open("sample.txt").read().strip().split("\n")))
input = list(map(list, open("input.txt").read().strip().split("\n")))
print(part1(sample))
print(part1(input))


def part2(map):
    total = 0
    removed = True
    while removed:
        removed = False
        height = len(map) - 1
        width = len(map[0]) - 1
        for x, row in enumerate(map):
            for y, _ in enumerate(row):
                neighbors = 0
                if map[x][y] == "@":
                    if x > 0 and y > 0 and map[x - 1][y - 1] == "@":
                        neighbors += 1
                    if x > 0 and map[x - 1][y] == "@":
                        neighbors += 1
                    if x > 0 and y < height and map[x - 1][y + 1] == "@":
                        neighbors += 1
                    if y < height and map[x][y + 1] == "@":
                        neighbors += 1
                    if x < width and y < height and map[x + 1][y + 1] == "@":
                        neighbors += 1
                    if x < width and map[x + 1][y] == "@":
                        neighbors += 1
                    if x < width and y > 0 and map[x + 1][y - 1] == "@":
                        neighbors += 1
                    if y > 0 and map[x][y - 1] == "@":
                        neighbors += 1
                    if neighbors < 4:
                        map[x][y] = "."
                        removed = True
                        total += 1
    mapprint(map)
    return total


sample = list(map(list, open("sample.txt").read().strip().split("\n")))
print(part2(sample))
input = list(map(list, open("input.txt").read().strip().split("\n")))
print(part2(input))

import re
import sys


def parse(iter):
    return [re.sub(r"[^\d.]", "@", line.strip()) for line in iter]


def check(map: list[str], start: tuple[int, int], end: tuple[int, int]) -> bool:
    maxrow = len(map)
    maxcol = len(map[0])

    for row in range(max(0, start[0] - 1), min(end[0] + 2, maxrow)):
        if "@" in map[row][max(0, start[1] - 1) : min(end[1] + 2, maxcol)]:
            return True
    return False


RED = "\x1b[41m"
GREEN = "\x1b[42m"
RESET = "\x1b[0m"


def summap(map: list[str], debug=False):
    sum = 0
    maxcol = len(map[0])
    for row in range(len(map)):
        col = 0
        while col < maxcol:
            if map[row][col].isdigit():
                start = col
                while col <= maxcol:
                    if col == maxcol or not map[row][col].isdigit():
                        if check(map, (row, start), (row, col - 1)):
                            sum += int(map[row][start:col])
                            if debug:
                                sys.stdout.write(f"{GREEN}{map[row][start:col]}{RESET}")
                        elif debug:
                            sys.stdout.write(f"{RED}{map[row][start:col]}{RESET}")
                        break
                    col += 1
            else:
                if debug:
                    sys.stdout.write(map[row][col])
                col += 1
        if debug:
            sys.stdout.write("\n")
    return sum


print(summap(parse(open("sample.txt"))))
print(summap(parse(open("input.txt"))))

import re


def parse(iter):
    return [re.sub(r"[^\d.]", "@", line.strip()) for line in iter]


def check(map: list[str], row: int, span: tuple[int, int]) -> bool:
    maxrow = len(map)
    maxcol = len(map[0])

    for row in range(max(0, row - 1), min(row + 2, maxrow)):
        if "@" in map[row][max(0, span[0] - 1) : min(span[1] + 1, maxcol)]:
            return True
    return False


def summap(map: list[str]):
    sum = 0
    for row in range(len(map)):
        for match in re.finditer(r"\d+", map[row]):
            if check(map, row, match.span()):
                sum += int(match.group())
    return sum


print("sample:", summap(parse(open("sample.txt"))))
print("part 1:", summap(parse(open("input.txt"))))

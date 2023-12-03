import re


def parse(iter):
    return [re.sub(r"[^*\d]", ".", line.strip()) for line in iter]


def adjacent(span: tuple[int, int], pos: int) -> bool:
    if span[0] < pos and span[1] > pos:
        return True
    if abs(span[0] - pos) < 2 or abs(span[1] - pos) < 1:
        return True
    return False


def nabes(map: list[str], pos: tuple[int, int]) -> list[int]:
    maxrow = len(map)

    ns = []
    # for the row above and below pos
    for row in range(max(0, pos[0] - 1), min(pos[0] + 2, maxrow)):
        for match in re.finditer(r"\d+", map[row]):
            if adjacent(match.span(), pos[1]):
                ns.append(int(match.group()))

    return ns


def summap(map: list[str]) -> int:
    sum = 0
    for r, row in enumerate(map):
        for match in re.finditer(r"\*", row):
            ns = nabes(map, (r, match.span()[0]))
            if len(ns) == 2:
                sum += ns[0] * ns[1]

    return sum


print("sample:", summap(parse(open("sample.txt"))))
print("part 2:", summap(parse(open("input.txt"))))

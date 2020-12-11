import ipdb
from pprint import pprint as pp
from copy import deepcopy

m = [list(l.strip()) for l in open("small.txt")]
m = [list(l.strip()) for l in open("input.txt")]

OCCUPIED = "#"
EMPTY = "L"
NONSEAT = "."


def get(m, x, y):
    if 0 <= y < len(m):
        if 0 <= x < len(m[0]):
            return m[y][x]
    return None


coords = [(x, y) for x in (0, 1, -1) for y in (0, 1, -1) if (x, y) != (0, 0)]

# If a seat is empty (L) and there are no occupied seats adjacent to it, the
# seat becomes occupied.
# return True if all adjacent seats are empty
def adj_unoccupied(m, x, y):
    # print(list(get(m, x + dx, y + dy) in [EMPTY, NONSEAT, None] for dx, dy in coords))
    return all(get(m, x + dx, y + dy) in [EMPTY, NONSEAT, None] for dx, dy in coords)


# If a seat is occupied (#) and four or more seats adjacent to it are also
# occupied, the seat becomes empty.
def becomes_empty(m, x, y):
    return sum(get(m, x + dx, y + dy) in [OCCUPIED] for dx, dy in coords) > 3


def pmap(m):
    for line in m:
        print("".join(line))
    print("--------")


rows = range(len(m))
cols = range(len(m[0]))

while 1:
    # pmap(m)
    # input("enter to continue")
    # ipdb.set_trace()
    changes = []
    for y in rows:
        for x in cols:
            if m[y][x] == EMPTY and adj_unoccupied(m, x, y):
                changes.append((x, y, OCCUPIED))
            if m[y][x] == OCCUPIED and becomes_empty(m, x, y):
                changes.append((x, y, EMPTY))
    if not changes:
        break
    for (x, y, seat) in changes:
        m[y][x] = seat

print(f"{sum(l.count(OCCUPIED) for l in m)} seats occupied")

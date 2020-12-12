import ipdb

m = [list(l.strip()) for l in open("small.txt")]
m = [list(l.strip()) for l in open("input.txt")]

OCCUPIED = "#"
EMPTY = "L"
NONSEAT = "."

h = len(m)
w = len(m[0])


def get(m, x, y):
    if 0 <= y < h:
        if 0 <= x < w:
            return m[y][x]
    return None


coords = [(x, y) for x in (0, 1, -1) for y in (0, 1, -1) if (x, y) != (0, 0)]

# returns true if there is a visible occupied seat from x y in the direction of
# dx, dy
def occupied_visible(m, x, y, dx, dy):
    x += dx
    y += dy
    while 0 <= y < h and 0 <= x < w:
        if m[y][x] == OCCUPIED:
            return True
        # do we need to distinguish between empty seats and non-seats or
        # running off the border?
        if m[y][x] == EMPTY:
            return False
        x += dx
        y += dy
    return False


# return true if an empty seat will be taken - if there are no occupied seats
# visible in any direction from that seat
def adj_unoccupied(m, x, y):
    return all(not occupied_visible(m, x, y, dx, dy) for dx, dy in coords)


# If a seat is occupied (#) and five or more seats adjacent to it are also
# occupied, the seat becomes empty.
def becomes_empty(m, x, y):
    return sum(occupied_visible(m, x, y, dx, dy) for dx, dy in coords) > 4


def pmap(m):
    for line in m:
        print("".join(line))
    print("--------")


rows = range(h)
cols = range(w)

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

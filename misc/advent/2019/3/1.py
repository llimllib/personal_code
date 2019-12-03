import numpy as np


instruction_sets = [
    [(cmd[0], int(cmd[1:])) for cmd in line.strip().split(",")]
    for line in open("a.txt")
]

FIELD_SIZE = 50000
port = [FIELD_SIZE // 2, FIELD_SIZE // 2]
field = np.zeros((FIELD_SIZE, FIELD_SIZE), dtype=np.uint8)
for i, instruction_set in enumerate(instruction_sets):
    cur = [FIELD_SIZE // 2, FIELD_SIZE // 2]
    # We're going to assign a wire number of 1 to the first wire, and 2 to the
    # second wire. Then we'll |= each field element with the wire number, and
    # any intersection will have a value of 3
    wire = i + 1
    field[cur[0], cur[1]] = 8
    for cmd, size in instruction_set:
        if cmd == "R":
            start = cur[1] + 1
            field[cur[0], start : start + size] |= wire
            cur[1] += size
        if cmd == "L":
            start = cur[1]
            field[cur[0], start - size : start] |= wire
            cur[1] -= size
        if cmd == "U":
            start = cur[0]
            field[start - size : start, cur[1]] |= wire
            cur[0] -= size
        if cmd == "D":
            start = cur[0] + 1
            field[start : start + size, cur[1]] |= wire
            cur[0] += size
        # good for debugging the small array
        # print(field, cmd, size)
        # input()

intersections = np.where(field == 3)
distances = [
    (abs(port[0] - m[0]) + abs(port[1] - m[1]), m) for m in zip(*intersections)
]
print(list(sorted(distances))[:10])

# not a great visualization on the big grid, but it works nicely on the small one
from PIL import Image


def trim2d(arr):
    # derived from
    # http://numpy-discussion.10968.n7.nabble.com/trim-zeros-in-more-than-one-dimension-tp7607p7610.html
    #
    # first, trim all rows with any nonzero value, then trim any columns
    arr = arr[:, arr.any(axis=0)]
    arr = arr[arr.any(axis=1), :]
    return arr


img = trim2d(field) * (255 // field.max())
im = Image.fromarray(img)
im.save("grid.png")

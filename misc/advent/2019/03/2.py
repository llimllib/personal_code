import numpy as np

instruction_sets = [
    [(cmd[0], int(cmd[1:])) for cmd in line.strip().split(",")]
    for line in open("a.txt")
]

FIELD_SIZE = 100000
port = [FIELD_SIZE // 2, FIELD_SIZE // 2]
field = np.zeros((FIELD_SIZE, FIELD_SIZE), dtype=np.uint32)
cur = [FIELD_SIZE // 2, FIELD_SIZE // 2]
field[cur[0], cur[1]] = 8

# first, trace out the first wire
step = 1
for cmd, size in instruction_sets[0]:
    if cmd == "R":
        for _ in range(size):
            cur[1] += 1
            field[cur[0], cur[1]] = field[cur[0], cur[1]] or step
            step += 1
    if cmd == "L":
        for _ in range(size):
            cur[1] -= 1
            field[cur[0], cur[1]] = field[cur[0], cur[1]] or step
            step += 1
    if cmd == "U":
        for _ in range(size):
            cur[0] -= 1
            field[cur[0], cur[1]] = field[cur[0], cur[1]] or step
            step += 1
    if cmd == "D":
        for _ in range(size):
            cur[0] += 1
            field[cur[0], cur[1]] = field[cur[0], cur[1]] or step
            step += 1

# now trace the second wire and note intersections and their total value, but
# don't add anything to the array
cur = [FIELD_SIZE // 2, FIELD_SIZE // 2]
step = 1
intersections = []
for cmd, size in instruction_sets[1]:
    if cmd == "R":
        for _ in range(size):
            cur[1] += 1
            curval = field[cur[0], cur[1]]
            if curval:
                intersections.append((step + curval, field[cur[0], cur[1]], step))
            step += 1
    elif cmd == "L":
        for _ in range(size):
            cur[1] -= 1
            curval = field[cur[0], cur[1]]
            if curval:
                intersections.append((step + curval, field[cur[0], cur[1]], step))
            step += 1
    elif cmd == "U":
        for _ in range(size):
            cur[0] -= 1
            curval = field[cur[0], cur[1]]
            if curval:
                intersections.append((step + curval, field[cur[0], cur[1]], step))
            step += 1
    elif cmd == "D":
        for _ in range(size):
            cur[0] += 1
            curval = field[cur[0], cur[1]]
            if curval:
                intersections.append((step + curval, field[cur[0], cur[1]], step))
            step += 1

print(list(sorted(intersections))[:10])

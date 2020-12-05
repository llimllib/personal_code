lines = [l for l in open("input.txt")]


def row(s):
    rowmin = 0
    rowmax = 127
    for char in s:
        diff = (rowmax - rowmin) + 1
        if char == "F":
            rowmax = rowmax - (diff // 2)
        else:
            rowmin = rowmin + (diff // 2)
    assert rowmin == rowmax
    return rowmin


def col(s):
    rowmin = 0
    rowmax = 7
    for char in s:
        diff = (rowmax - rowmin) + 1
        if char == "L":
            rowmax = rowmax - (diff // 2)
        else:
            rowmin = rowmin + (diff // 2)
    assert rowmin == rowmax
    return rowmin


print(row("FBFBBFF"))
print(col("RLR"))

ids = []
for line in lines:
    r = row(line[:7])
    c = col(line[7:])
    ids.append(r * 8 + c)

print(max(ids))
print(sorted(ids))
ids = sorted(ids)
for i in range(1, len(ids) - 1):
    if ids[i - 1] + 2 == ids[i]:
        print(i, ids[i])

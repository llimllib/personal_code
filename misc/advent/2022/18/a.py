sample = """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""


def parse(text):
    return {
        (int(a), int(b), int(c))
        for a, b, c in [l.split(",") for l in text.strip().split("\n")]
    }


def check(pts):
    exposed = 0
    for x, y, z in pts:
        if (x + 1, y, z) not in pts:
            exposed += 1
        if (x - 1, y, z) not in pts:
            exposed += 1
        if (x, y + 1, z) not in pts:
            exposed += 1
        if (x, y - 1, z) not in pts:
            exposed += 1
        if (x, y, z + 1) not in pts:
            exposed += 1
        if (x, y, z - 1) not in pts:
            exposed += 1
    return exposed


assert check(parse(sample)) == 64
print(check(parse(open("input.txt").read())))

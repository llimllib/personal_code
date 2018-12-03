def intersect(r1, r2):
    x1, y1, x2, y2, id1 = r1
    x3, y3, x4, y4, id2 = r2

    # if the left of r2 is right of r1: x2 < x3
    # or the right of r2 is left of r1: x4 < x1
    # or the bottom of r2 is above the top of r1
    # or the top of r2 is below the bottom of r1
    if x2 < x3 or x4 < x1 or y2 < y3 or y4 < y1:
        return False
    return True


def do(f):
    rectangles = []
    for line in f:
        id, _, xy, size = line.split()
        id = int(id[1:])
        x, y = map(int, xy.strip(':').split(','))
        w, h = map(int, size.split('x'))
        rect = (x, y, x + w, y + h, id)
        rectangles.append(rect)

    for r in rectangles:
        try:
            for r2 in rectangles:
                if r2 == r:
                    continue
                if intersect(r, r2):
                    raise Exception()
            print(f"{r}")
        except:
            continue


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))

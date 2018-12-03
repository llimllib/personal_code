def intersect(r1, r2):
    x1, y1, x2, y2, id1 = r1
    x3, y3, x4, y4, id2 = r2

    # make x1 the left-most rectangle
    if x3 < x1:
        x1, y1, x2, y2, x3, y3, x4, y4 = x3, y3, y4, y4, x1, y1, x2, y2

    # if they don't intersect, there's no overlap:
    if not (x1 < x3 < x2 and (y1 < y3 < y2 or y1 < y4 < y2)):
        # print("skipping ", id1, id2)
        return 0

    # if the second rect is contained in the first, the x overlap is its len
    xoverlap = min(x4, x2) - x3

    if y1 < y3:
        yoverlap = min(y4, y2) - y3
    else:
        yoverlap = y4 - y1

    # print("overlap: ", xoverlap, yoverlap, id1, id2)
    return xoverlap * yoverlap


def do(f):
    rectangles = []
    sum = 0
    for line in f:
        id, _, xy, size = line.split()
        id = int(id[1:])
        x, y = map(int, xy.strip(':').split(','))
        w, h = map(int, size.split('x'))
        rect = (x, y, x + w, y + h, id)
        for rectangle in rectangles:
            sum += intersect(rectangle, rect)
        rectangles.append(rect)
    print(sum)


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))

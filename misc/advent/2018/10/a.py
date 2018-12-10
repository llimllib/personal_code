import re


def display(pts, t):
    maxx = max(pts)[0]
    maxy = max(pts, key=lambda x: x[1])[1]
    minx = min(pts)[0]
    miny = min(pts, key=lambda x: x[1])[1]
    xmag = maxx - minx
    ymag = maxy - miny
    if xmag > 100 or ymag > 100:
        return
    grid = [[' '] * (xmag + 1) for _ in range(ymag + 1)]
    for px, py, _, _ in pts:
        grid[py - miny][px - minx] = '*'
    print("\n".join("".join(row) for row in grid))
    print(t)
    input()


def do(infile):
    pts = []
    for line in infile:
        (px, py), (vx, vy) = re.findall(r"<\s*([\-\d]+),\s*([\-\d]+)", line)
        pts.append((int(px), int(py), int(vx), int(vy)))

    t = 0
    while 1:
        pts = [(px + vx, py + vy, vx, vy) for px, py, vx, vy in pts]
        display(pts, t)
        t += 1


if __name__ == "__main__":
    #do(open("small.txt"))
    do(open("input.txt"))

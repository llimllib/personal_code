import sys

map_ = [list(l) for l in open(sys.argv[1]).read().split("\n") if l]
w = len(map_[0])


def search(xinc, yinc):
    x, y = (0, 0)
    trees = 0
    while 1:
        if y >= len(map_):
            break
        try:
            if map_[y][x] == "#":
                trees += 1
        except IndexError:
            print(y, x)
            break
        x = (x + xinc) % w
        y = y + yinc
    return trees


print(search(3, 1))
print(search(3, 1) * search(1, 1) * search(5, 1) * search(7, 1) * search(1, 2))

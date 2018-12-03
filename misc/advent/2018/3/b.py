def do(f):
    fabric = [[''] * 1000 for _ in range(1000)]
    for line in f:
        id, _, xy, size = line.split()
        id = int(id[1:])
        x, y = map(int, xy.strip(':').split(','))
        w, h = map(int, size.split('x'))
        for i in range(x, x + w):
            for j in range(y, y + h):
                if not fabric[i][j]:
                    fabric[i][j] = '.'
                else:
                    fabric[i][j] = '#'
    print(sum(row.count('#') for row in fabric))


if __name__ == "__main__":
    #do(open("small.txt"))
    do(open("input.txt"))

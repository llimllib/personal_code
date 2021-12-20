from copy import deepcopy


def pad(arr, n=1, pad=0):
    w = len(arr[0])
    wp = w + 2 * n
    padded = [[pad] * wp for _ in range(n)]
    for line in arr:
        padded.append([pad] * n + line + [pad] * n)
    padded.extend([[pad] * wp for _ in range(n)])
    return padded


def parse(blob, padding):
    repl, im = blob.split("\n\n")
    repl = list(1 if c == "#" else 0 for c in "".join(repl.split("\n")))
    assert len(repl) == 512

    image = []
    for line in im.split("\n"):
        if not line:
            continue
        image.append(list((1 if c == "#" else 0 for c in line.strip())))

    return (repl, pad(image, n=padding))


def show(im):
    print()
    for line in im:
        print("".join("█" if c else "░" for c in line))


def neighbors(im, row, col, default=0):
    w = len(im)
    h = len(im[0])
    for radj in (-1, 0, 1):
        for cadj in (-1, 0, 1):
            r = row + radj
            c = col + cadj
            if r >= 0 and c >= 0 and r < h and c < w:
                yield im[r][c]
            else:
                yield default


def tobin(ns):
    return int("".join(str(i) for i in ns), 2)


def convolve(repl, im, iterations=1):
    imp = None
    default = 0
    for _ in range(iterations):
        imp = deepcopy(im)
        for row in range(len(im)):
            for col in range(len(im[0])):
                idx = tobin(neighbors(im, row, col, default))
                imp[row][col] = repl[idx]

        im = imp
        default = repl[-1] if default else repl[0]
    return imp


def count(im):
    n = 0
    for row in range(len(im)):
        for col in range(len(im[0])):
            if im[row][col]:
                n += 1
    return n


def do(f, iterations):
    im = convolve(*parse(open(f).read(), iterations), iterations)
    show(im)
    print(count(im))


if __name__ == "__main__":
    do("small.txt", 2)
    do("input.txt", 2)
    do("small.txt", 50)
    do("input.txt", 50)

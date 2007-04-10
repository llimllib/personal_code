import sys

def parse(stdin):
    n_prob = int(stdin.readline().strip())
    out = []
    for i in range(n_prob):
        whites = {}
        rows, cols = map(int, stdin.readline().strip().split())
        x = 0
        y = 0
        for i in range(rows):
            line = stdin.readline()
            for c in line:
                if c == "1": whites[(x, i)] = None
                x += 1
            x = 0
        out.extend(closest(rows, cols, whites))
        stdin.readline()
    print "".join(out)

def closest(rows, cols, whites):
    #.888 with this one (fastest so far)
    out = []
    for j in range(rows):
        for i in range(cols):
            if (i, j) in whites: out.append("0")
            else: out.append(str(min(abs(w[0] - i) + abs(w[1] - j) for w in whites.iterkeys())))
            out.append(" ")
        out.append("\n")
    return out

parse(sys.stdin)

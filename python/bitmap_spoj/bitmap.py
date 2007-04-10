import sys

def parse(stdin):
    n_prob = int(stdin.readline().strip())
    for i in range(n_prob):
        whites = set()
        rows, cols = map(int, stdin.readline().strip().split())
        x = 0
        y = 0
        for i in range(rows):
            line = stdin.readline()
            for c in line:
                if c == "1": whites.add((x, i))
                x += 1
            x = 0
        closest(rows, cols, whites)
        stdin.readline()

def closest(rows, cols, whites):
    out = []
    for j in range(rows):
        for i in range(cols):
            if (i, j) in whites: out.append("0")
            else: out.append(str(min(abs(w[0] - i) + abs(w[1] - j) for w in whites)))
            out.append(" ")
        out.append("\n")
    print "".join(out)

parse(sys.stdin)

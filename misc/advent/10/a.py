from collections import Counter

lines = [int(l) for l in open("a.txt")]
lines = [int(l) for l in open("b.txt")]
lines = [int(l) for l in open("input.txt")]

lines.append(0)

slines = list(sorted(lines))
c = Counter([slines[i + 1] - n for (i, n) in enumerate(slines) if i < len(slines) - 1])
print(c[1], c[3] + 1, c[1] * (c[3] + 1))

# [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19]
#    1 3  1  1  1  3   1   1   3   1   3
# 7 4

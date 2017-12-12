txt = open("input.txt").read().strip()
sum = 0
for l in txt.split("\n"):
    bits = l.split("\t")
    d = max([int(x) for x in bits if x]) - min([int(x) for x in bits if x])
    sum += d
print(sum)


from itertools import combinations
sum = 0
for l in txt.split("\n"):
    bits = [int(bit) for bit in l.split("\t")]
    rowfound = False
    for b1, b2 in combinations(bits, 2):
        big = max(b1, b2)
        small = min(b1, b2)
        if big % small == 0:
            sum += big // small
            print(big, small, sum, l)
            break
print(sum)

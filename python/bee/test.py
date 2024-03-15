test = frozenset("equivoke")
# test = frozenset("cazique")
n = 0
for word in set(line.strip() for line in open("words.txt") if len(line.strip()) > 3):
    if test.issuperset(frozenset(word)):
        n += 1
        print(word)
print(n)

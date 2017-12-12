text = open("input.txt").read().strip().split("\n")

c = 0
for line in text:
    l = line.split(" ")
    if len(set(l)) == len(l):
        c += 1
print(c)

c = 0
for line in text:
    words = line.split(" ")
    setwords = set(frozenset(word) for word in words)
    if len(words) == len(setwords):
        c += 1
print(c)

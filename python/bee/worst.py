from collections import Counter
from itertools import combinations
from pprint import pprint

allwords = set(line.strip() for line in open("words.txt"))
score = Counter()
alphabet = frozenset("abcdefghijklmnopqrstuvwxyz")
for word in allwords:
    ws = frozenset(word)
    if 3 < len(word) and len(ws) < 7:
        for letters in combinations(alphabet - ws, 7 - len(ws)):
            score[ws.union(letters)] += 1

pangrams = [
    (score[frozenset(line.strip())] + 1, line.strip())
    for line in open("words.txt")
    if len(set(line.strip())) == 7
]
pangrams.sort()
pprint(pangrams[:15])

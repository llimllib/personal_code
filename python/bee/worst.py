# worst.py: print the worst possible pangrams for the New York Times spelling bee

from collections import Counter
from itertools import combinations
import sys

FILE = sys.argv[-1] if sys.argv[-1].endswith(".txt") else "words.txt"

# the spelling bee never includes the letter "s"
alphabet = frozenset("abcdefghijklmnopqrtuvwxyz")
allwords = set(line.strip() for line in open(FILE) if "s" not in line)


count = Counter()
score = Counter()
for word in allwords:
    lw = len(word)
    ws = frozenset(word)
    lws = len(ws)
    if lw > 3 and lws <= 7:
        if lws < 7:
            for letters in combinations(alphabet - ws, 7 - lws):
                count[ws.union(letters)] += 1
                score[ws.union(letters)] += lw if lw > 4 else 1
        else:
            count[ws] += 1
            score[ws] += 7 + (lw if lw > 4 else 1)

pangrams = [
    (score[frozenset(word)], count[frozenset(word)] + 1, word)
    for word in [line.strip() for line in open(FILE)]
    if "s" not in word and len(set(word)) == 7
]
pangrams.sort()

red = "\033[0;31m"
green = "\033[0;32m"
blue = "\033[0;34m"
reset = "\033[0m"
print(f"{green}words\tpoints\tpangram")
for points, n, pangram in pangrams[:15]:
    ps = set(pangram)
    matches = [
        w
        for w in [l.strip() for l in open(FILE)]
        if set(w).issubset(ps) and len(w) > 3 and w != pangram
    ]
    matchstr = ",".join(matches)
    if len(matchstr) > 60:
        matchstr = matchstr[:59] + "..."
    print(f"{blue}{n: <8}{points: <8}{red}{pangram: <15}{reset}{matchstr}")

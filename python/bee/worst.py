# worst.py: print the worst possible pangrams for the New York Times spelling bee

from collections import Counter
from itertools import combinations

# the spelling bee never includes the letter "s"
alphabet = frozenset("abcdefghijklmnopqrtuvwxyz")
allwords = set(line.strip() for line in open("words.txt") if "s" not in line)

score = Counter()
for word in allwords:
    ws = frozenset(word)
    if 3 < len(word) and len(ws) < 7:
        for letters in combinations(alphabet - ws, 7 - len(ws)):
            score[ws.union(letters)] += 1

pangrams = [
    (score[frozenset(line.strip())] + 1, line.strip())
    for line in open("words.txt")
    if "s" not in line and len(set(line.strip())) == 7
]
pangrams.sort()

red = "\033[0;31m"
green = "\033[0;32m"
blue = "\033[0;34m"
reset = "\033[0m"
print(f"{green}words\tpangram")
for n, pangram in pangrams[:15]:
    ps = set(pangram)
    matches = ",".join(
        [
            w
            for w in [l.strip() for l in open("words.txt")]
            if set(w).issubset(ps) and len(w) > 3 and w != pangram
        ]
    )
    if len(matches) > 60:
        matches = matches[:59] + "..."
    print(f"{blue}{n: <8}{red}{pangram: <15}{reset}{matches}")

# worst.py: print the worst possible pangrams for the New York Times spelling bee

from collections import Counter
from functools import cache
from itertools import combinations
import sys

FILE = sys.argv[-1] if sys.argv[-1].endswith(".txt") else "words.txt"

# the spelling bee never includes the letter "s"
alphabet = frozenset("abcdefghijklmnopqrtuvwxyz")
allwords = set(
    line.strip()
    for line in open(FILE)
    if "s" not in line and len(line.strip()) > 3 and len(set(line.strip())) <= 7
)
allwordsets = {w: frozenset(w) for w in allwords}


# calculate the count of words and score for every pangram
count = Counter()
score = Counter()
allpangrams = []
for word in allwords:
    lw = len(word)
    ws = allwordsets[word]
    lws = len(ws)
    if lws <= 7:
        if lws < 7:
            for letters in combinations(alphabet - ws, 7 - lws):
                count[ws.union(letters)] += 1
                score[ws.union(letters)] += lw if lw > 4 else 1
        else:
            allpangrams.append(word)
            count[ws] += 1
            score[ws] += 7 + (lw if lw > 4 else 1)

# get the score and count of each pangram, and sort by total score
pangrams = [
    (score[allwordsets[word]], count[allwordsets[word]] + 1, word)
    for word in allpangrams
]
pangrams.sort()


@cache
def wscore(w):
    return (1 if len(w) == 4 else len(w)) + (7 if len(allwordsets[w]) == 7 else 0)


scores_with_req_letters = []
for points, _, pangram in pangrams:
    ps = allwordsets[pangram]
    matches = [
        w
        for w in allwords
        if allwordsets[w].issubset(ps) and len(w) > 3 and w != pangram
    ]
    for l in ps:
        lmatches = [m for m in matches if l in m]
        score = len(pangram) + 7 + sum(wscore(m) for m in lmatches)
        scores_with_req_letters.append((score, l, pangram))


@cache
def hl(w: str, l: str) -> str:
    return f"{red}{w.replace(l, f'{yellow}{l}{red}')}"


red = "\033[0;31m"
green = "\033[0;32m"
yellow = "\033[0;33m"
blue = "\033[0;34m"
reset = "\033[0m"
print(f"{green}points\tpangram\t\twords")
scores_with_req_letters.sort()
for points, l, pangram in scores_with_req_letters[:50]:
    ps = allwordsets[pangram]
    matches = [
        w
        for w in allwords
        if l in w and allwordsets[w].issubset(ps) and len(w) > 3 and w != pangram
    ]
    matchstr = ",".join(matches)
    if len(matchstr) > 60:
        matchstr = matchstr[:59] + "..."
    print(
        f"{blue}{points: <8}{hl(pangram, l)}{' ' * (16-len(pangram))}{reset}{matchstr}"
    )

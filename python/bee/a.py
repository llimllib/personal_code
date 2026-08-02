from collections import Counter
from itertools import combinations
import sys

FILE = sys.argv[-1] if sys.argv[-1].endswith(".txt") else "words3.txt"

# the spelling bee never includes the letter "s"
alphabet = frozenset("abcdefghijklmnopqrtuvwxyz")
allwords = set(
    line.strip()
    for line in open(FILE)
    if "s" not in line and len(line.strip()) > 3 and len(set(line.strip())) <= 7
)
allwordsets = {w: frozenset(w) for w in allwords}


# calculate the count of words and score for every pangram
score = Counter()
allpangrams = []
for word in allwords:
    lw = len(word)
    ws = allwordsets[word]
    lws = len(ws)
    if lws < 7:
        for letters in combinations(alphabet - ws, 7 - lws):
            score[ws.union(letters)] += lw if lw > 4 else 1
    else:
        allpangrams.append(word)
        score[ws] += 7 + (lw if lw > 4 else 1)

# get the score and count of each pangram, and sort by total score
pangrams = [(score[allwordsets[word]], word) for word in allpangrams]

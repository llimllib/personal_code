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
pangrams = [(word, ws) for word, ws in allwordsets.items() if len(ws) == 7]

score = Counter()
for pangram, ps in pangrams:
    for word, ws in allwordsets.items():
        if ws.issubset(ps):
            if len(ws) == 7:
                score[pangram] += 7 + len(word)
            elif len(word) == 4:
                score[pangram] += 1
            else:
                score[pangram] += len(word)

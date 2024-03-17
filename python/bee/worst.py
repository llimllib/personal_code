# worst.py: print the worst possible pangrams for the New York Times spelling bee

from collections import Counter
from functools import cache
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


# find all matches for each pangram, then the score for each possible required
# letter
scores_with_req_letters = []
pangram_matches = {}
for points, _, pangram in pangrams:
    ps = allwordsets[pangram]
    matches = [w for w in allwords if allwordsets[w].issubset(ps) and w != pangram]
    for l in ps:
        lmatches = [m for m in matches if l in m]
        pangram_matches[(pangram, l)] = lmatches
        score = len(pangram) + 7 + sum(wscore(m) for m in lmatches)
        scores_with_req_letters.append((score, l, len(lmatches), pangram))


@cache
def hl(w: str, l: str) -> str:
    return f"{red}{w.replace(l, f'{yellow}{l}{red}')}"


red = "\N{esc}[0;31m"
green = "\N{esc}[0;32m"
yellow = "\N{esc}[0;33m"
blue = "\N{esc}[0;34m"
reset = "\N{esc}[0m"
print(f"{green}points\tpangram\t\twords")
# uncomment to print out markdown instead of ANSI
# print("| score | pangram | words |")
# print("|-------|---------|-------|")
scores_with_req_letters.sort()
for points, l, _, pangram in scores_with_req_letters:
    matches = pangram_matches[(pangram, l)]
    panscore = wscore(pangram)
    total = panscore + sum(wscore(m) for m in matches)
    if (panscore / total) > 0.7:
        matchstr = ",".join(matches)
        if len(matchstr) > 60:
            matchstr = matchstr[:59] + "..."
        print(
            f"{blue}{points: <8}{hl(pangram, l)}{' ' * (16-len(pangram))}{reset}{matchstr}"
        )
        # uncomment to print out markdown instead of ANSI
        # print(f'|{points}|{pangram.replace(l, f"**{l}**")}|{",".join(matches)}|')

print(f"{yellow}----------- lowest puzzles with >= 16 words --------------{reset}")

# print the 25 lowest-scoring puzzles which generate at least 20 words
for points, l, nwords, pangram in [s for s in scores_with_req_letters if s[2] >= 15][
    :25
]:
    matches = pangram_matches[(pangram, l)]
    panscore = wscore(pangram)
    total = panscore + sum(wscore(m) for m in matches)
    matchstr = ",".join(matches)
    if len(matchstr) > 60:
        matchstr = matchstr[:59] + "..."
    print(
        f"{blue}{points: <8}{hl(pangram, l)}{' ' * (16-len(pangram))}{reset}{matchstr}"
    )

print(
    f"{yellow}----------- the lowest ever actual puzzle, score 47 points --------------{reset}"
)
for points, l, nwords, pangram in [
    s for s in scores_with_req_letters if s[1] == "f" and s[3] == "mortify"
]:
    matches = pangram_matches[(pangram, l)]
    panscore = wscore(pangram)
    total = panscore + sum(wscore(m) for m in matches)
    matchstr = ",".join(matches)
    print(
        f"{blue}{points: <8}{hl(pangram, l)}{' ' * (16-len(pangram))}{reset}{matchstr}"
    )
    # uncomment to print out markdown instead of ANSI
    # print(f'|{points}|{pangram.replace(l, f"**{l}**")}|{",".join(matches)}|')

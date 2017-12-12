from itertools import chain
import re

patterns = open("input.txt").readlines()

ABBA = re.compile(r"(.)(?!\1)(.)\2\1")
def match_tls(s):
    groups = re.split("(\[.*?\])", s)
    return any(ABBA.search(g) for g in groups if not g.startswith('[')) \
            and not any(ABBA.search(g) for g in groups if g.startswith('['))

def flatten(listOfLists):
    "Flatten one level of nesting"
    return chain.from_iterable(listOfLists)

# lookahead allows the overlapping patterns to match, because it consumes only one character, not the whole match
ABA = re.compile(r"(?=(.)(?!\1)(.)\1)")
def match_ssl(s):
    groups = re.split("(\[.*?\])", s)
    abas = flatten(ABA.findall(g) for g in groups if not g.startswith('['))
    brace_groups = [g for g in groups if g.startswith('[')]
    for a,b in abas:
        for group in brace_groups:
            if "{}{}{}".format(b,a,b) in group: return True

    return False

print(sum(match_tls(s) for s in patterns))
print(sum(match_ssl(s) for s in patterns))

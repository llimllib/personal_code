import re
from collections import Counter
from itertools import groupby

rawrooms = open('rooms.txt').readlines()
rooms = (re.match(r'^([a-z\-]*)-(\d+)\[([a-z]{5})\]$', r).groups()
         for r in rawrooms)

def flipneg(iter_): return ((-t[1], t[0]) for t in iter_)
def rotate(c, n): return chr(96 + ((ord(c)-96 + (n%26)) % 26))
def rotateword(word, n): return ''.join(map(lambda c: rotate(c, n) if c != '-' else ' ', word))

i = 0
for name, roomid, cksum in rooms:
    c = Counter(name.replace('-', ''))
    v = ''.join(x[1] for x in sorted(flipneg(c.most_common())))[:5]
    if v == cksum:
        i += int(roomid)
    if 'pole' in rotateword(name, int(roomid)):
        print("Part 2: {}".format(roomid))
print("Part 1: {}".format(i))

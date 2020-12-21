from functools import reduce
from itertools import chain
from operator import mul
import re
from ipdb import set_trace as debug

rawtiles = [t.split("\n") for t in open("input.txt").read().strip().split("\n\n")]


def rev(s):
    return "".join(reversed(s))


# top, right, bottom, left
tileidx = {}
fulltiles = {}
for tile in rawtiles:
    _, n = tile[0].split()
    n = int(n.strip(":"))
    fulltiles[n] = "\n".join([r[1:9] for r in tile[2:10]])
    tileidx[n] = (
        tile[1],
        "".join([r[-1] for r in tile[1:]]),
        rev(tile[-1]),
        "".join([r[0] for r in tile[-1:0:-1]]),
    )

edges = {}
for n, tile in tileidx.items():
    for edge in tile:
        edges.setdefault(edge, []).append(n)
        edges.setdefault(rev(edge), []).append(n)

# picking out the corners is easy (because thankfully the puzzle does not have
# multiple tiles with the same edges as the outsides of the corners)
corners = []
for n, tile in tileidx.items():
    ones = 0
    for edge in tile:
        if len(edges[edge]) == 1:
            ones += 1
    if ones == 2:
        corners.append(n)
print(f"part 1: {reduce(mul, corners)}")


def hflip(tile):
    t, r, b, l = tile
    return (rev(t), rev(l), rev(b), rev(r))


def vflip(tile):
    t, r, b, l = tile
    return (rev(b), rev(t), rev(r), rev(l))


def rot(tile):
    t, r, b, l = tile
    return (l, t, r, b)


def left(tile):
    return tile[3]


def top(tile):
    return tile[0]


def strblk(block):
    return "\n".join("".join(c) for c in block)


def rotblk(block):
    block = [list(line) for line in block.strip().split("\n")]
    return strblk(list(zip(*block[::-1])))


def hflipblk(block):
    block = [list(line) for line in block.strip().split("\n")]
    return strblk([b[::-1] for b in block])


def flatten(list_of_lists):
    "Flatten one level of nesting"
    return chain.from_iterable(list_of_lists)


def matchtile(edge, tile):
    "return the tile that matches the given edge and is not tile"
    matches = [t for t in edges[edge] if t != tile]
    if not len(matches):
        raise Exception(f"Could not match {edge} for {tile}")
    return matches[0]


# given an edge to match, rotate tile until it matches at the given side function
def rotatetomatch(edge, tile, n, matcher=left):
    # if the edge under consideration is not in the tile, we need to flip it
    # prior to rotating
    if edge not in tile:
        tile = hflip(tile)
        fulltiles[n] = hflipblk(fulltiles[n])
    assert edge in tile
    # rotate the tile until its left matches the current right
    i = 0
    while matcher(tile) != edge:
        tile = rot(tile)
        fulltiles[n] = rotblk(fulltiles[n])
        i += 1
        assert i < 4
    return tile


# the board is a 12 x 12 square... I found the first row to figure this out
board = [[0] * 12 for _ in range(12)]

# let's start with a corner and try to build the puzzle out
# piece 1543 is the top-left corner, because its top and left edges are solos:
# In [87]: print([(e, edges[e]) for e in tileidx[1543]])
# [('#...#.#.#.', [1543]), ('..#.#.#.##', [1543, 2617]), ('#...#.#.##', [1543, 2273]), ('###.##...#', [1543])]
board[0][0] = 1543
for y in range(0, 12):
    for x in range(1, 12):
        # get the current tile's right edge
        _, r, _, _ = tileidx[board[y][x - 1]]
        match = matchtile(r, board[y][x - 1])

        # verify the match isn't already in
        assert match not in flatten(board)

        board[y][x] = match
        # now we need to rotate the tile so that it fits in our puzzle
        tile = rotatetomatch(rev(r), tileidx[board[y][x]], match)
        tileidx[board[y][x]] = tile

    # if there's a row below this row, seed the first entry
    if y < 11:
        # get the current row's bottom edge
        _, _, b, _ = tileidx[board[y][0]]
        match = matchtile(b, board[y][0])

        # verify the match isn't already in
        assert match not in flatten(board)

        board[y + 1][0] = match
        tile = rotatetomatch(rev(b), tileidx[match], match, matcher=top)
        tileidx[match] = tile


def joinrow(*args):
    return "\n".join("".join(x) for x in zip(*[y.split("\n") for y in args]))


# return a list of indexes of every occurence of needle in s
def findall(haystack, needle):
    idxs = []
    pat = re.compile(needle, re.M | re.S)
    match = pat.search(haystack)
    while match:
        start, _ = match.span()
        idxs.append(start)
        match = pat.search(haystack, pos=start + 1)
    return idxs


# now we have the whole board! Let's join up all the interiors
fullboard = ""
for row in board:
    fullboard += joinrow(*[fulltiles[r] for r in row]) + "\n"

# NOW we have a 96x96 board ready to search for dragons! Does 96x96 make sense
# - it does. 8 x 12 = 96
boards = [
    fullboard,
    rotblk(fullboard),
    rotblk(rotblk(fullboard)),
    rotblk(rotblk(rotblk(fullboard))),
    hflipblk(fullboard),
    rotblk(hflipblk(fullboard)),
    rotblk(rotblk(hflipblk(fullboard))),
    rotblk(rotblk(rotblk(hflipblk(fullboard)))),
]

#           1111111111
# 01234567890123456789
#                   #
# #    ##    ##    ###
#  #  #  #  #  #  #
def countn(board):
    l = len(board.split("\n")[0])
    n = len(
        findall(board, f"#.{{{l-18}}}#....##....##....###.{{{l-18}}}#..#..#..#..#..#")
    )
    # this doesn't work beceause it doesn't find overlapping matches! took me forever to recognize that
    # n = 0
    # for match in re.finditer(
    #     f"#.{{{l-18}}}#....##....##....###.{{{l-18}}}#..#..#..#..#..#",
    #     board,
    #     re.M | re.S,
    # ):
    #     start, end = match.span()
    #     print(start, end)
    #     n += 1

    return n


assert countn(open("sample.txt").read()) == 2


maxn = 0
for i, board in enumerate(boards):
    n = countn(board)
    print(i, n)
    if maxn < n:
        maxn = n
    if n > maxn:
        maxn = n

# Guess: 18xx  your answer is too high
# Guess: 1689 (15 monsters) too high still
# Guess: 1629 (19 monsters) too high still
# Guess: 1614 not right (no longer providing hints, wait 5 minutes)
# finally got the right answer! 1599
print(f"part 2: {fullboard.count('#') - (maxn * 15)}")

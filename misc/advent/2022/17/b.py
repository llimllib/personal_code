from functools import reduce
from itertools import cycle
from operator import mul
import sys
import time

import ipdb
import numpy as np

# rock rows in binary, followed by rock width
ROCKS = [
    ([15], 4),  # 0b1111
    ([2, 7, 2], 3),  # 010, 111, 010
    ([1, 1, 7], 3),  # 001, 001, 111
    ([1, 1, 1, 1], 1),  # 1, 1, 1, 1, 1
    ([3, 3], 2),  # 11, 11
]


def rocks():
    i = 0
    while True:
        yield ROCKS[i % len(ROCKS)]
        i += 1


def rockp(rows, pos, rock, rockw):
    rrow, rcol = pos
    for r, row in enumerate(rows):
        for c, bit in enumerate("{0:07b}".format(row)):
            if int(bit):
                sys.stdout.write("#")
            elif rrow <= r < rrow + len(rock) and rock[r - rrow] << (
                7 - rockw - rcol
            ) & 1 << (6 - c):
                sys.stdout.write("@")
            else:
                sys.stdout.write(".")
        sys.stdout.write("\n")


def collision(board, row, col, rock, rockw):
    for i, r in enumerate(rock):
        if board[row + i] & (r << (7 - rockw - col)):
            return True
    return False


def run(jets, n):
    height = 100_000_000
    row = height
    minrow = height

    cyccheck = set()

    board = np.zeros(height, dtype=np.ubyte)
    rockgen = rocks()
    rockn = 0

    jetn = 0
    jetl = len(jets)
    jets = cycle(jets)

    fout = open("cycle.out", "w")

    while n > 0:
        # Each rock appears so that its left edge is two units away from the
        # left wall and its bottom edge is three units above the highest rock
        # in the room (or the floor, if there isn't one).
        col = 2

        rock, rockw = next(rockgen)
        rockh = len(rock)
        rockn += 1
        row = minrow - 3 - rockh

        while True:
            # rockp(board[height - 30 :], (row - (height - 30), col), rock, rockw)
            # val = input()
            # print("\033[2J")
            # if "t" in val:
            #     ipdb.set_trace()

            # apply the jet
            jet = next(jets)
            jetn += 1
            if (
                jet == ">"
                and col + rockw < 7
                and not collision(board, row, col + 1, rock, rockw)
            ):
                col += 1
            elif (
                jet == "<"
                and col > 0
                and not collision(board, row, col - 1, rock, rockw)
            ):
                col -= 1

            # if we've hit the floor, stop
            if rockh + row + 1 > height:
                break

            # if we cannot move down
            if collision(board, row + 1, col, rock, rockw):
                break

            # if we got here, move the rock down
            row += 1

        # place the rock. It can get placed below the current "highest" rock,
        # so minrow is either where it got placed or the row where the previous
        # highest rock got placed
        minrow = min(row, minrow)
        for i, r in enumerate(rock):
            board[row + i] |= r << (7 - rockw - col)
            if i == 0 and board[row] == 127:

                if list(board[row + 1 : row + 5]) == [62, 16, 16, 16]:
                    fout.write(f"{rockn},")
                    fout.flush()
                # if list(board[row + 1 : row + 5]) == [62, 16, 16, 16]:
                #     print("------", jetn, rockn, board[row : row + 10])
                #     rockp(board[row : row + 30], (-100, -100), rock, rockw)

        n -= 1

    return height - minrow


def cyccalc(goal, start, cyclesize):
    a = run(inp, start)
    goal -= start

    # the size of each cycle
    b = run(inp, start + cyclesize) - a

    # the remainder is: goal - (start + goal*cycles*cyclesize)
    remain = goal - (goal // cyclesize) * cyclesize

    c = run(inp, start + cyclesize + remain)
    return a + (goal // cyclesize) * b + (c - (b + a))


inp = list(open("input.txt").read().strip())

# discovered that after cycle 1266, the cycle repeats every 1695 blocks

assert cyccalc(1_000_000, 1266, 1695) == 1575829
print(cyccalc(1000000000000, 1266, 1695))

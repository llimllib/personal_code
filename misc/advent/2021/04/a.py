import math
from utils import flatten, printmat


def parse(f):
    ns = list(map(float, next(f).strip().split(",")))
    boards = []
    for line in f:
        line = line.strip()
        if not line:
            boards.append([])
            continue
        boards[-1].append(list(map(float, line.split())))
    return ns, boards[:-1]


def negz(n):
    """return true if n < 0 or n == -0.0"""
    # is there an easier way to check for -0.0? there's no math.sign
    return n < 0 or math.copysign(1.0, n) < 0


def markboards(n, boards):
    for board in boards:
        for row in board:
            for i in range(len(board)):
                if row[i] == n:
                    row[i] = -row[i]


def winner(board):
    l = len(board)
    for r in range(l):
        if all(negz(board[r][i]) for i in range(l)):
            return True
    for c in range(l):
        if all(negz(board[i][c]) for i in range(l)):
            return True


def bingo(ns, boards):
    l = len(boards[0][0])
    for n in ns:
        markboards(n, boards)

        for board in boards:
            if winner(board):
                return n, board


def score(n, board):
    return sum(x for x in flatten(board) if x >= 0) * n


print(score(*bingo(*parse(open("small.txt")))))
print(score(*bingo(*parse(open("input.txt")))))


def badbingo(ns, boards):
    l = len(boards[0][0])
    for n in ns:
        markboards(n, boards)

        liveboards = []
        for board in boards:
            if winner(board):
                if len(boards) == 1:
                    return n, board
                continue
            liveboards.append(board)

        boards = liveboards


print(score(*badbingo(*parse(open("small.txt")))))
print(score(*badbingo(*parse(open("input.txt")))))

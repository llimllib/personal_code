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


def markboards(n, boards):
    for board in boards:
        for row in board:
            for i in range(len(board)):
                if row[i] == n:
                    row[i] = -1


def winner(board):
    l = len(board)
    for r in range(l):
        if all(board[r][i] < 0 for i in range(l)):
            return True
    for c in range(l):
        if all(board[i][c] < 0 for i in range(l)):
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

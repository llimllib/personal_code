from collections import defaultdict
import sys


def move(pos, direction):
    if direction == "E":
        return (pos[0], pos[1] + 1)
    elif direction == "W":
        return (pos[0], pos[1] - 1)
    elif direction == "N":
        return (pos[0] - 1, pos[1])
    elif direction == "S":
        return (pos[0] + 1, pos[1])


def get(grid, move):
    return grid[move[0]][move[1]]


def go(inp):
    grid = tuple(tuple(line.strip("\n")) for line in inp)

    pos = (0, grid[0].index('|'))
    direction = "S"
    letters = []
    steps = 0
    while 1:
        if pos[0] < 0 or pos[1] < 0:
            raise ValueError
        if get(grid, pos) == " ":
            print(''.join(letters), steps)
            return
        print((pos[0] + 1, pos[1] + 1), direction, letters)
        nextpos = move(pos, direction)

        if get(grid, nextpos) == "+":
            turn = {
                "S": get(grid, move(nextpos, "S")),
                "N": get(grid, move(nextpos, "N")),
                "E": get(grid, move(nextpos, "E")),
                "W": get(grid, move(nextpos, "W"))
            }
            found = False
            for key in turn:
                if (key != direction and
                    (direction in ("N", "S") and turn[key] == '-')
                        or (direction in ("E", "W") and turn[key] == "|")):
                    nextpos = move(nextpos, key)
                    steps += 1
                    direction = key
                    found = True
                    break
            if not found:
                print(''.join(letters))
                return
        elif get(grid, nextpos).isalpha():
            letters.append(get(grid, nextpos))
        pos = nextpos
        steps += 1


if __name__ == "__main__":
    go(open("input.txt"))
    #go(open("small.txt"))

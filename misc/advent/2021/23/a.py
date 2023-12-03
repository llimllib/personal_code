# the second row of the map is the hallway
HALLWAY = 1
FRONT = 2
BACK = 3

OPEN = "."

ROOMS = [3, 5, 7, 9]


def parse(it):
    mapp = []
    for line in it:
        if not line.strip():
            continue
        mapp.append(list(line.rstrip()))
    return mapp


def players(mapp):
    locs = []
    for row in range(len(mapp)):
        for col in range(len(mapp[row])):
            if mapp[row][col] in "ABCD":
                locs.append((row, col))
    return locs


def valid_moves(mapp):
    # a move is a tuple (from_row, from_col, to_row, to_col)
    moves = []
    for row, col in players(mapp):
        print(row, col)
        player = mapp[row][col]
        # players in the rooms can move to any hallway spot that is not
        # immediately outside their room, as long as there's nobody blocking
        # them in their room
        if row != HALLWAY and row == FRONT or mapp[FRONT][col] == OPEN:
            for i in range(1, len(mapp[HALLWAY]) - 1):
                if mapp[HALLWAY][i] == OPEN and i != col:
                    moves.append((row, col, HALLWAY, i))
        # players in the hallway can move into a room if it's empty or contains
        # only their same kind (and has an empty spot, but that must be true
        # given our puzzles... if there were ever more than two of one kind
        # this would need to be explicit)
        elif row == HALLWAY:
            for room in ROOMS:
                if mapp[FRONT][room] == mapp[BACK][room] == ".":
                    moves.append((row, col, FRONT, room))
                elif mapp[FRONT][room] == player:
                    moves.append((row, col, BACK, room))
                elif mapp[BACK][room] == player:
                    moves.append((row, col, FRONT, room))
    return moves


def search(mapp):
    frontier = valid_moves(mapp)
    print(frontier)


search(
    parse(
        """
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
""".split(
            "\n"
        )
    )
)

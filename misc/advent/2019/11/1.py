from cpu import cpu

BLACK = 0
WHITE = 1


def color(col):
    if col != BLACK and col != WHITE:
        raise Exception(f"invalid color {col}")
    return "white" if col else "black"


TURN_LEFT = 0
TURN_RIGHT = 1


def direction(turn):
    if turn != TURN_LEFT and turn != TURN_RIGHT:
        raise Exception(f"invalid turn {turn}")
    return "left" if turn == TURN_LEFT else "right"


UP = 0
LEFT = 1
DOWN = 2
RIGHT = 3

acpu = cpu([int(n) for n in open("in").read().split(",")], [])
panels = {}
loc = (0, 0)
dir = UP
painted = 0

# start the cpu, it runs to the first input
while 1:
    next(acpu)
    # run until it wants input
    print(f"sending {panels.get(loc, BLACK)}")
    paint = acpu.send(panels.get(loc, BLACK))
    turn = next(acpu)
    print(f"got {color(paint)}, {direction(turn)}")

    if panels.get(loc, BLACK) != paint:
        print(f"painting {loc} {color(paint)}")
        panels[loc] = paint
        painted += 1

    if turn == TURN_LEFT:
        dir = dir + 1 % 3
    elif turn == TURN_RIGHT:
        dir = dir - 1 % 3

    if dir == UP:
        print("moving up")
        loc = (loc[0], loc[1] - 1)
    elif dir == LEFT:
        print("moving left")
        loc = (loc[0] - 1, loc[1])
    elif dir == RIGHT:
        print("moving right")
        loc = (loc[0] + 1, loc[1])
    elif dir == UP:
        print("moving up")
        loc = (loc[0], loc[1] + 1)
    print(loc)

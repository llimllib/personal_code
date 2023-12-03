# this doesn't work yet, but I think the strategy is right and I just need to
# find where it isn't quite right
from functools import cache
from typing import Tuple


@cache
def move(cur, roll):
    return ((cur + roll - 1) % 10) + 1


def up(twotuple, player, n) -> Tuple[int, int]:
    if player == 0:
        return (n, twotuple[1])
    return (twotuple, n)


# states is a map of:
# {(p1 position, p2 position): {(p1 score, p2 score): count}}
#
# the idea being, there's a small state space to the game so we can just track
# how many games are in each state
states = {
    (i, j): {(k, l): 0 for k in range(21) for l in range(21)}
    for i in range(1, 11)
    for j in range(1, 11)
}

wins = [0, 0]

# the initial state
states[(4, 8)][(0, 0)] += 1

# each player on each turn rolls 1 3, 3 4s, 6 5s, 7 6s, 6 7s, 3 8s, and a 9
rolls = ((3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1))

n = 0
while 1:
    n += 1
    newstates = {
        (i, j): {(k, l): 0 for k in range(21) for l in range(21)}
        for i in range(1, 11)
        for j in range(1, 11)
    }
    player = 0
    # for every board state
    for positions, scores in states.items():
        # for every score state at that board position (where there exists any
        # games)
        for score, state_count in [
            (k, count) for k, count in scores.items() if count > 0
        ]:
            # for every possible roll for each player
            for roll, roll_count in rolls:
                print(positions, roll)
                positions = up(positions, player, move(positions[player], roll))
                print(positions, roll)
                print("----")
                if score[player] + positions[player] >= 21:
                    wins[player] += state_count * roll_count
                    continue

                score = up(score, player, positions[player] + score[player])
                newstates[positions][score] += state_count * roll_count

    # if there are non-empty states left, carry on
    for state, scores in newstates.items():
        if any(scores[k] for k in scores):
            break
    else:
        # if we didn't find any scores, quit the main loop
        break

    # debugging
    for state, scores in newstates.items():
        for (p1, p2), count in scores.items():
            if count:
                print(f"{state} -> ({p1}, {p2}): {count}")

    print(wins)
    break

    states = newstates
    player = 0 if player else 1

print(wins, n)

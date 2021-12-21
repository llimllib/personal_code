# this doesn't work yet, but I think the strategy is right and I just need to
# find where it isn't quite right
from itertools import groupby
from functools import cache

from utils import flatten


@cache
def move(cur, roll):
    return ((cur + roll - 1) % 10) + 1


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

p1wins = 0
p2wins = 0

# the initial state
states[(4, 8)][(0, 0)] += 1

# each player on each turn rolls 1 3, 3 4s, 6 5s, 7 6s, 6 7s, 3 8s, and a 9
rolls = list(flatten([[3], [4] * 3, [5] * 6, [6] * 7, [7] * 6, [8] * 3, [9]]))
pair_rolls = [(a, b) for a in rolls for b in rolls]

# which means that the list of roll pairs, and how often they occur, looks like:
# [((3, 3), 1), ((3, 4), 3), ((3, 5), 6), ((3, 6), 7), ((3, 7), 6),
#    ((3, 8), 3), ((3, 9), 1),
#  ((4, 3), 3), ((4, 4), 9), ((4, 5), 18), ((4, 6), 21), ((4, 7), 18),
#    ((4, 8), 9), ((4, 9), 3),
#  ((5, 3), 6), ((5, 4), 18), ((5, 5), 36), ((5, 6), 42), ((5, 7), 36),
#    ((5, 8), 18), ((5, 9), 6),
#  ((6, 3), 7), ((6, 4), 21), ((6, 5), 42), ((6, 6), 49), ((6, 7), 42),
#    ((6, 8), 21), ((6, 9), 7),
#  ((7, 3), 6), ((7, 4), 18), ((7, 5), 36), ((7, 6), 42), ((7, 7), 36),
#    ((7, 8), 18), ((7, 9), 6),
#  ((8, 3), 3), ((8, 4), 9), ((8, 5), 18), ((8, 6), 21), ((8, 7), 18),
#    ((8, 8), 9), ((8, 9), 3),
#  ((9, 3), 1), ((9, 4), 3), ((9, 5), 6), ((9, 6), 7), ((9, 7), 6),
#    ((9, 8), 3), ((9, 9), 1)
# ]
rolls = list((a, len(list(b))) for a, b in groupby(sorted(pair_rolls)))

n = 0
while 1:
    n += 1
    newstates = {
        (i, j): {(k, l): 0 for k in range(21) for l in range(21)}
        for i in range(1, 11)
        for j in range(1, 11)
    }
    # for every board state
    for (p1pos, p2pos), scores in states.items():
        # for every score state at that board position (where there exists any
        # games)
        for (p1score, p2score), state_count in [
            (k, count) for k, count in scores.items() if count > 0
        ]:
            # for every possible roll for each player
            for (roll1, roll2), roll_count in rolls:
                newp1 = move(p1pos, roll1)
                if p1score + newp1 >= 21:
                    p1wins += state_count * roll_count
                    continue

                # do the same thing for p2
                newp2 = move(p2pos, roll2)
                if p2score + newp2 >= 21:
                    p2wins += state_count * roll_count
                    continue

                newstates[(newp1, newp2)][(p1score + newp1, p2score + newp2)] += (
                    state_count * roll_count
                )

    # if there are non-empty states left, carry on
    for state, scores in newstates.items():
        if any(scores[k] for k in scores):
            break
    else:
        # if we didn't find any scores, quit the main loop
        break

    # debugging
    # if n > 2:
    #     for state, scores in newstates.items():
    #         for (p1, p2), count in scores.items():
    #             if count:
    #                 print(f"{state} -> ({p1}, {p2}): {count}")

    #     print(p1wins, p2wins)
    #     break

    states = newstates

print(p1wins, p2wins, n)

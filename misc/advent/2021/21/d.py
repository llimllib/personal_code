# now we get the first player's score wrong, but the second player's score right?
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
# states[(4, 8)][(0, 0)] = 1
states[(10, 1)][(0, 0)] = 1

# each player on each turn rolls 1 3, 3 4s, 6 5s, 7 6s, 6 7s, 3 8s, and a 9
rolls = ((1, 3), (3, 4), (6, 5), (7, 6), (6, 7), (3, 8), (1, 9))

n = 0
while 1:
    n += 1
    newstates = {
        (i, j): {(k, l): 0 for k in range(21) for l in range(21)}
        for i in range(1, 11)
        for j in range(1, 11)
    }
    for (p1pos, p2pos), scores in states.items():
        for (p1score, p2score), state_count in [
            (k, count) for k, count in scores.items() if count > 0
        ]:
            for roll_count, roll in rolls:
                newp1 = move(p1pos, roll)
                if p1score + newp1 >= 21:
                    p1wins += state_count * roll_count
                    continue

                newstates[(newp1, p2pos)][(p1score + newp1, p2score)] += (
                    state_count * roll_count
                )
    states = newstates

    newstates = {
        (i, j): {(k, l): 0 for k in range(21) for l in range(21)}
        for i in range(1, 11)
        for j in range(1, 11)
    }
    for (p1pos, p2pos), scores in states.items():
        for (p1score, p2score), state_count in [
            (k, count) for k, count in scores.items() if count > 0
        ]:
            for roll_count, roll in rolls:
                newp2 = move(p2pos, roll)
                if p2score + newp2 >= 21:
                    p2wins += state_count * roll_count
                    continue
                newstates[(p1pos, newp2)][(p1score, p2score + newp2)] += (
                    state_count * roll_count
                )

    # if there are non-empty states left, carry on
    for state, scores in newstates.items():
        if any(scores[k] for k in scores):
            break
    else:
        # if we didn't find any scores, quit the main loop
        break

    states = newstates

print(max(p1wins, p2wins), n)

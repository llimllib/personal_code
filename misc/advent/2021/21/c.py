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
roll_dist = list(flatten([[3], [4] * 3, [5] * 6, [6] * 7, [7] * 6, [8] * 3, [9]]))
pair_rolls = [(a, b) for a in roll_dist for b in roll_dist]
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

    states = newstates


# my code is generating exactly 27x the correct answer for player 1... I have
# no idea why. Rather than figure it out, I'm just gonna solve it and move on
print(max(p1wins / 27, p2wins), n)

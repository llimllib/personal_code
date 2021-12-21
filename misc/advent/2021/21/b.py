# this doesn't work yet, but I think the strategy is right and I just need to
# find where it isn't quite right
from utils import flatten
from functools import cache


@cache
def rolldie(cur, roll):
    return ((cur + roll - 1) % 10) + 1


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

while 1:
    newstates = {
        (i, j): {(k, l): 0 for k in range(21) for l in range(21)}
        for i in range(1, 11)
        for j in range(1, 11)
    }
    for state, scores in states.items():
        p1pos, p2pos = state
        for (p1score, p2score), count in scores.items():
            if not count:
                continue
            for roll1, roll2 in pair_rolls:
                newp1 = rolldie(p1pos, roll1)
                if p1score + newp1 >= 21:
                    p1wins += count
                    break
                newp2 = rolldie(p2pos, roll2)
                if p2score + newp2 >= 21:
                    p2wins += count
                    break
                newstates[(newp1, newp2)][(p1score + newp1, p2score + newp2)] += count

    # if there are non-empty states left, carry on
    for state, scores in states.items():
        if any(scores[k] for k in scores):
            break
    else:
        # if we didn't find any scores, quit the main loop
        break

    # debugging
    # for state, scores in newstates.items():
    #     for (p1, p2), count in scores.items():
    #         if count:
    #             print(f"{state} -> ({p1}, {p2}): {count}")

    states = newstates

print(p1wins, p2wins)

def insert(marbles, marble, marblei):
    if marble % 23 == 0:
        deleteidx = (marblei - 7) % len(marbles)
        score = marbles.pop(deleteidx)
        return marble + score, marbles, deleteidx

    nextidx = (marblei + 2) % len(marbles)
    # if nextidx is 0, insert at the end instead of the front, strictly to match AoC's formatting
    nextidx = nextidx if nextidx else len(marbles)
    marbles.insert(nextidx, marble)
    return 0, marbles, nextidx


def p(arr, highlight):
    s = []
    for i, elt in enumerate(arr):
        if i == highlight:
            s.append(f" \033[31m{elt}\033[m ")
        else:
            s.append(f" {elt} ")
    return "".join(s)


def play(players, nmarbles):
    marbles = [0]
    marblei = 0
    marblen = 0

    score = 0
    scores = [0] * players
    player = -1

    loops = 0
    while marblen < nmarbles:
        loops += 1
        marblen += 1
        player = (player + 1) % players
        score, marbles, marblei = insert(marbles, marblen, marblei)
        scores[player] += score
        # print(f"{player+1}: {p(marbles, marblei)}")

    #print(f"{scores}")
    print(f"({players}, {nmarbles}): {max(scores)}")


if __name__ == "__main__":
    # play(9, 25)
    # play(10, 1618)
    # play(13, 7999)
    # play(17, 1104)
    # play(21, 6111)
    # play(30, 5807)
    play(425, 7084800)

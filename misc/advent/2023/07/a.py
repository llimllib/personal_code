import sys
from collections import Counter


def parse(iter):
    hands = []
    faces = {"T": 10, "J": 11, "Q": 12, "K": 13, "A": 14}
    for line in iter:
        hand, bid = line.strip().split(" ")
        hand = [faces.get(c) or int(c) for c in hand]
        hands.append((hand, int(bid)))
    return hands


def score(hand):
    hand, _ = hand
    (_, n), *rest = sorted(
        Counter(hand).items(), key=lambda x: (x[1], x[0]), reverse=True
    )

    if n == 5:
        return (7, *hand)
    (_, nn) = rest.pop(0)
    if n == 4:
        return (6, *hand)
    if n == 3 and nn == 2:
        return (5, *hand)
    if n == 3:
        return (4, *hand)
    if n == 2 and nn == 2:
        return (3, *hand)
    if n == 2:
        return (2, *hand)
    return (1, *hand)


hands = parse(sys.stdin)
print(sum((i + 1) * bid for i, (_, bid) in enumerate(sorted(hands, key=score))))

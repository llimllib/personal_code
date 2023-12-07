import sys
from collections import Counter


def parse(iter):
    hands = []
    faces = {"T": 10, "J": 1, "Q": 12, "K": 13, "A": 14}
    for line in iter:
        hand, bid = line.strip().split(" ")
        hand = [int(faces.get(c, c)) for c in hand]
        hands.append((hand, int(bid)))
    return hands


def score(hand):
    hand, _ = hand
    js = hand.count(1)
    (top, n), *rest = sorted(
        Counter(hand).items(), key=lambda x: (x[1], x[0]), reverse=True
    )
    if top == 1:
        # if the hand is entirely jacks, rest will be empty
        n = rest.pop(0)[1] if len(rest) else 0

    if n + js == 5:
        return (7, *hand)
    (_, nn) = rest.pop(0) if len(rest) else (0, 0)
    if n + js == 4:
        return (6, *hand)
    if n + nn + js == 5:
        return (5, *hand)
    if n + js == 3:
        return (4, *hand)
    if n + nn + js == 4:
        return (3, *hand)
    if n + js == 2:
        return (2, *hand)
    return (1, *hand)


hands = parse(sys.stdin)
print(sum((i + 1) * bid for i, (_, bid) in enumerate(sorted(hands, key=score))))

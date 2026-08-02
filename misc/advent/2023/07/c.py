import sys
from collections import Counter

print(
    sum(
        (i + 1) * bid
        for i, (_, bid) in enumerate(
            sorted(
                [
                    (
                        [
                            {"T": 10, "J": 11, "Q": 12, "K": 13, "A": 14}.get(c)
                            or int(c)
                            for c in cards
                        ],
                        int(bid),
                    )
                    for cards, bid in [line.split(" ") for line in sys.stdin]
                ],
                key=lambda hb: list(sorted(Counter(hb[0]).values(), reverse=True))
                + hb[0],
            )
        )
    )
)

from dataclasses import dataclass
from itertools import cycle
from typing import Iterator


@dataclass
class State:
    p1pos: int
    p2pos: int
    p1score: int
    p2score: int
    die: Iterator[int]


def turn(state: State) -> int:
    state.p1pos = (
        (state.p1pos + next(state.die) + next(state.die) + next(state.die) - 1) % 10
    ) + 1
    state.p1score += state.p1pos
    if state.p1score >= 1000:
        return 3

    state.p2pos = (
        (state.p2pos + next(state.die) + next(state.die) + next(state.die) - 1) % 10
    ) + 1
    state.p2score += state.p2pos

    return 6


def game(state):
    rolls = 0
    while state.p1score < 1000 and state.p2score < 1000:
        rolls += turn(state)
    print(min((state.p1score, state.p2score)) * rolls)


if __name__ == "__main__":
    die = cycle(range(1, 101))
    game(State(4, 8, 0, 0, die))
    die = cycle(range(1, 101))
    game(State(10, 1, 0, 0, die))

# this is an unfinished attempt to use multiprocessing
from itertools import islice
from multiprocessing import Pool
import re


def parse(text):
    return [
        tuple(map(int, re.findall(r"([\-\d]+)", line)))
        for line in text.strip().split("\n")
    ]


def h(x):
    return x[1] + x[3] * 2 + x[5] * 30 + x[7] ** 200


def search(bp):
    print(bp)
    (
        _,
        ore_cost,
        clay_cost,
        obs_ore_cost,
        obs_clay_cost,
        geode_ore_cost,
        geode_obs_cost,
    ) = bp
    #         0         1       2        3       4       5         6          7
    #        ore, ore_robots, clay, clay_robos, obs, obs_robots, geodes, geode_robots
    state = (0, 1, 0, 0, 0, 0, 0, 0)
    frontier = {state}
    for minute in range(1, 25):
        newfrontier = set()

        if minute > 19:
            frontier = set(islice(sorted(frontier, key=h, reverse=True), 1_000_000))

        while frontier:
            state = frontier.pop()
            if state[0] >= ore_cost:
                newfrontier.add(
                    (
                        state[0] + state[1] - ore_cost,
                        state[1] + 1,
                        state[2] + state[3],
                        state[3],
                        state[4] + state[5],
                        state[5],
                        state[6] + state[7],
                        state[7],
                    )
                )
            if state[0] >= clay_cost:
                newfrontier.add(
                    (
                        state[0] + state[1] - clay_cost,
                        state[1],
                        state[2] + state[3],
                        state[3] + 1,
                        state[4] + state[5],
                        state[5],
                        state[6] + state[7],
                        state[7],
                    )
                )
            if state[0] >= obs_ore_cost and state[2] >= obs_clay_cost:
                newfrontier.add(
                    (
                        state[0] + state[1] - obs_ore_cost,
                        state[1],
                        state[2] + state[3] - obs_clay_cost,
                        state[3],
                        state[4] + state[5],
                        state[5] + 1,
                        state[6] + state[7],
                        state[7],
                    )
                )
            if state[0] >= geode_ore_cost and state[4] >= geode_obs_cost:
                newfrontier.add(
                    (
                        state[0] + state[1] - geode_ore_cost,
                        state[1],
                        state[2] + state[3],
                        state[3],
                        state[4] + state[5] - geode_obs_cost,
                        state[5],
                        state[6] + state[7],
                        state[7] + 1,
                    )
                )

            newfrontier.add(
                (
                    state[0] + state[1],
                    state[1],
                    state[2] + state[3],
                    state[3],
                    state[4] + state[5],
                    state[5],
                    state[6] + state[7],
                    state[7],
                )
            )
        frontier = newfrontier
    return max(frontier, key=lambda x: x[6])


sample = """
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"""
prints = parse(sample)
# result = search(prints[0])
# print(result)
# print(result[6])
# result = search(prints[1])
# print(result)
# print(result[6])

results = [(p[0], search(p)[6]) for p in prints]
print(results)

assert results[0][1] == 9
assert results[1][1] == 12
assert sum(a * b for a, b in results) == 33

prints = parse(open("input.txt").read())
with Pool(processes=4) as pool:
    results = pool.map(search, prints)
print(results)

# 979 is too low
print(sum(a * b for a, b in results))

import copy
import ipdb
import re


def parse(text):
    return [
        tuple(map(int, re.findall(r"([\-\d]+)", line)))
        for line in text.strip().split("\n")
    ]


def update_resources(state):
    state["ore"] += state["ore_robots"]
    state["clay"] += state["clay_robots"]
    state["obsidian"] += state["obs_robots"]
    state["geode"] += state["geode_robots"]


def search(bp):
    (
        _,
        ore_cost,
        clay_cost,
        obs_ore_cost,
        obs_clay_cost,
        geode_ore_cost,
        geode_obs_cost,
    ) = bp
    state = {
        "ore": 0,
        "ore_robots": 1,
        "clay": 0,
        "clay_robots": 0,
        "obsidian": 0,
        "obs_robots": 0,
        "geode": 0,
        "geode_robots": 0,
        "history": (),
    }
    frontier = [state]
    for minute in range(1, 25):
        # print(minute)
        newfrontier = []

        if minute > 16:
            frontier.sort(
                key=lambda x: x["obs_robots"] ** 4 + (x["geode_robots"] + 1) ** 4,
                reverse=True,
            )
            frontier = frontier[:1000]

        while frontier:
            state = frontier.pop()
            if state["ore"] >= ore_cost:
                st = copy.copy(state)
                update_resources(st)
                st["ore_robots"] += 1
                st["ore"] -= ore_cost
                # st["history"] += (
                #     f"{minute}: built ore robot {st['ore']} {st['clay']} {st['obsidian']} {st['geode']}",
                # )
                newfrontier.append(st)
            if state["ore"] >= clay_cost:
                st = copy.copy(state)
                update_resources(st)
                st["clay_robots"] += 1
                st["ore"] -= clay_cost
                # st["history"] += (
                #     f"{minute}: built clay robot {st['ore']} {st['clay']} {st['obsidian']} {st['geode']}",
                # )
                newfrontier.append(st)
            if state["ore"] >= obs_ore_cost and state["clay"] >= obs_clay_cost:
                st = copy.copy(state)
                update_resources(st)
                st["obs_robots"] += 1
                st["ore"] -= obs_ore_cost
                st["clay"] -= obs_clay_cost
                # st["history"] += (
                #     f"{minute}: built obsidian robot {st['ore']} {st['clay']} {st['obsidian']} {st['geode']}",
                # )
                newfrontier.append(st)
            if state["ore"] >= geode_ore_cost and state["obsidian"] >= geode_obs_cost:
                st = copy.copy(state)
                update_resources(st)
                st["geode_robots"] += 1
                st["ore"] -= ore_cost
                st["obsidian"] -= geode_obs_cost
                # st["history"] += (
                #     f"{minute}: built geode robot {st['ore']} {st['clay']} {st['obsidian']} {st['geode']}",
                # )
                newfrontier.append(st)

            update_resources(state)

            newfrontier.append(state)
        frontier = newfrontier
    return max(frontier, key=lambda x: x["geode"])


sample = """
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"""
prints = parse(sample)
result = search(prints[1])
for line in result["history"]:
    print(line)
print(result)
print(result["geode"])

assert search(prints[0])["geode"] == 9
assert search(prints[1])["geode"] == 24

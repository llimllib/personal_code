colors = {"red": 0, "green": 1, "blue": 2}


def parse(iter):
    games = {}
    for line in iter:
        game, bags = line.split(":")
        _, gid = game.split(" ")
        gid = int(gid)
        games[gid] = []
        for bag in bags.split(";"):
            diceCounts = [0, 0, 0]
            for dice in bag.split(","):
                n, color = dice.strip().split(" ")
                diceCounts[colors[color]] += int(n)
            games[gid].append(diceCounts)

    return games


def possible(games, limit=(12, 13, 14)):
    return sum(
        id
        for id, games in games.items()
        if not any(g[0] > limit[0] for g in games)
        and not any(g[1] > limit[1] for g in games)
        and not any(g[2] > limit[2] for g in games)
    )


def powersum(games, limit=(12, 13, 14)):
    return sum(
        max(g[0] for g in games) * max(g[1] for g in games) * max(g[2] for g in games)
        for games in games.values()
    )


print(
    possible(
        parse(
            """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".split(
                "\n"
            )
        )
    )
)

print(possible(parse(open("input.txt"))))


print(
    powersum(
        parse(
            """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".split(
                "\n"
            )
        )
    )
)

print(powersum(parse(open("input.txt"))))

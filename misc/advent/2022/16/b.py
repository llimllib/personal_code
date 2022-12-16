import re


def parse(text):
    inp = re.findall(r"Valve (\w\w).*?rate=(\d+).*?valves? (.*)", text)
    flows = dict((start, int(rate)) for start, rate, _ in inp)
    graph = dict((start, nabes.replace(" ", "").split(",")) for start, _, nabes in inp)

    return graph, flows


def options(graph, flows, loc, path, opened):
    options = []
    for nabe in graph[loc]:
        options.append((path + [nabe], opened.copy()))

    if flows[loc] > 0 and loc not in opened:
        options.append((path, opened | {loc}))

    return options


def find(graph, flows, start):
    frontier = [(0, ([start], [start]), set(), [])]
    for i in range(1, 27):
        # extremely janky heuristic of just keeping the top-scoring 3000
        # entries
        if i > 5:
            frontier.sort(reverse=True)
            frontier = frontier[:3000]
        newfrontier = []
        for pressure, paths, opened, debug in frontier:
            explorer = paths[0][-1]
            elephant = paths[1][-1]

            p = sum(flows[o] for o in opened)
            pressure += p
            if p > 0:
                debug += [f"{i}: releasing {p} pressure"]

            for pex, opex in options(graph, flows, explorer, paths[0], opened):
                for pel, opel in options(graph, flows, elephant, paths[1], opex):
                    newfrontier.append((pressure, [pex, pel], opel, debug[:]))

        frontier = newfrontier
    return max(frontier)


graph, flows = parse(open("sample.txt").read())
pressure, path, opened, debug = find(graph, flows, "AA")
# for a in debug:
#     print(a)
assert pressure == 1707

graph, flows = parse(open("input.txt").read())
pressure, path, opened, debug = find(graph, flows, "AA")
print("part 2:", pressure)

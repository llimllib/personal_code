import re


def parse(text):
    inp = re.findall(r"Valve (\w\w).*?rate=(\d+).*?valves? (.*)", text)
    flows = dict((start, int(rate)) for start, rate, _ in inp)
    graph = dict((start, nabes.replace(" ", "").split(",")) for start, _, nabes in inp)

    return graph, flows


def find(graph, flows, start):
    frontier = [(0, [start], set(), [])]
    for i in range(1, 31):
        # extremely janky heuristic of just keeping the top-scoring 3000
        # entries
        if i > 5:
            frontier.sort(reverse=True)
            frontier = frontier[:3000]
        newfrontier = []
        for pressure, path, opened, debug in frontier:
            loc = path[-1]

            p = sum(flows[o] for o in opened)
            pressure += p
            if p > 0:
                debug.append(f"{i}: releasing {p} pressure")

            # at this step, we could travel to a neighbor
            for nabe in graph[loc]:
                nd = debug + [f"{i}: moving to {nabe}"]
                newfrontier.append((pressure, path + [nabe], opened.copy(), nd))

            # alternately, we could release the pressure
            if flows[loc] > 0 and loc not in opened:
                debug.append(f"{i}: opening {loc}")
                newfrontier.append((pressure, path, opened | {loc}, debug))

        frontier = newfrontier
    return max(frontier)


graph, flows = parse(open("sample.txt").read())
pressure, path, opened, debug = find(graph, flows, "AA")
# for a in debug:
#     print(a)
assert pressure == 1651

graph, flows = parse(open("input.txt").read())
pressure, path, opened, debug = find(graph, flows, "AA")
print("part 1:", pressure)

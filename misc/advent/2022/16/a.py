import heapq
import re


def parse(text):
    inp = re.findall(r"Valve (\w\w).*?rate=(\d+).*?valves? (.*)", text)
    flows = dict((start, int(rate)) for start, rate, _ in inp)
    graph = dict((start, nabes.replace(" ", "").split(",")) for start, _, nabes in inp)

    return graph, flows


def find(graph, flows, start):
    frontier = [(0, [start], set())]
    for i in range(30):
        print(i)
        newfrontier = []
        for pressure, path, opened in frontier:
            # at this step, we could travel to a neighbor
            for nabe in graph[path[-1]]:
                newfrontier.append((pressure, path + [nabe], opened))
            # alternately, we could release the pressure
            if pressure > 0 and path[-1] not in opened:
                newfrontier.append(
                    (pressure + flows[path[-1]], path, opened + [path[-1]])
                )
        frontier = newfrontier
    return max(frontier)


graph, flows = parse(open("sample.txt").read())
best = find(graph, flows, "AA")

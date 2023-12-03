import re


def parse(text):
    inp = re.findall(r"Valve (\w\w).*?rate=(\d+).*?valves? (.*)", text)
    flows = dict((start, int(rate)) for start, rate, _ in inp)
    graph = dict((start, nabes.replace(" ", "").split(",")) for start, _, nabes in inp)

    return graph, flows


def options(graph, flows, loc, path, opened, debug, name, step):
    options = []
    for nabe in graph[loc]:
        nd = debug + (f"{step}: {name} moved from {loc} to {nabe}",)
        options.append((path + [nabe], opened.copy(), nd))

    if flows[loc] > 0 and loc not in opened:
        nd = debug + (f"{step}: {name} opened {loc}",)
        options.append((path, opened | {loc}, nd))

    return options


def draw(graph, flows, loc, opened):
    out = open("graph.dot", "w")
    out.write("digraph G {\n    rankdir=LR;\n")
    for node, children in graph.items():
        for child in children:
            flow = flows[node]
            if node == loc:
                out.write(f'    {node}[color="red",label="{flow}"]')
            elif node in opened:
                out.write(f'    {node}[color="blue",label="{flow}"]')
            else:
                out.write(f'    {node}[label="{node} - {flow}"]')
            out.write(f"    {node} -> {child};")
    out.write("}")


def find(graph, flows, start):
    frontier = [(0, ([start], [start]), set(), ())]
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

            for pex, opex, debug in options(
                graph, flows, explorer, paths[0], opened, debug, "explorer", i
            ):
                for pel, opel, debug in options(
                    graph, flows, elephant, paths[1], opex, debug, "elephant", i
                ):
                    newfrontier.append((pressure, [pex, pel], opel, debug))

        frontier = newfrontier
    return max(frontier)


graph, flows = parse(open("input.txt").read())
draw(graph, flows, "AA", {})
pressure, path, opened, debug = find(graph, flows, "AA")
for d in debug:
    print(d)
# print("part 2:", pressure)

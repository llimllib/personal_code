from collections import defaultdict


def parse(it):
    graph = defaultdict(list)
    for line in it:
        if not line.strip():
            continue
        a, b = line.strip().split("-")
        graph[a].append(b)
        graph[b].append(a)

    # remove edges to the start node
    for key in graph:
        try:
            graph[key].remove("start")
        except ValueError:
            pass

    # remove edges from the end node
    del graph["end"]

    return graph


def paths(graph):
    paths = [("start",)]
    complete = set()
    while paths:
        newpaths = []
        for path in paths:
            for node in graph[path[-1]]:
                if node == "end":
                    complete.add(path + ("end",))
                elif node.islower() and node not in path:
                    newpaths.append(path + (node,))
                elif node.isupper():
                    newpaths.append(path + (node,))
        paths = newpaths
    return len(complete)


def nodupe(path):
    """return true if there is no duplicate lower-cased letter in the path"""
    ls = [p for p in path if p.islower()]
    return len(ls) == len(set(ls))


def dubbpaths(graph):
    paths = [("start",)]
    complete = set()
    while paths:
        newpaths = []
        for path in paths:
            for node in graph[path[-1]]:
                if node == "end":
                    complete.add(path + ("end",))
                elif node.islower():
                    if node not in path or nodupe(path):
                        newpaths.append(path + (node,))
                else:
                    newpaths.append(path + (node,))
        paths = newpaths
    return len(complete)


print(paths(parse(open("small.txt"))))
print(paths(parse(open("med.txt"))))
print(paths(parse(open("lg.txt"))))
print("part a ---> ", paths(parse(open("input.txt"))))
print(dubbpaths(parse(open("small.txt"))))
print(dubbpaths(parse(open("med.txt"))))
print(dubbpaths(parse(open("lg.txt"))))
print("part b ---> ", dubbpaths(parse(open("input.txt"))))

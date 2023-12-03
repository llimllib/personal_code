from collections import defaultdict
from typing import Iterator, List, Tuple, Set


def parse(it: Iterator[str]) -> defaultdict[str, List[str]]:
    graph: defaultdict[str, List[str]] = defaultdict(list)
    line: str
    for line in it:
        if not line.strip():
            continue
        a: str
        b: str
        a, b = line.strip().split("-")
        graph[a].append(b)
        graph[b].append(a)

    # remove edges to the start node
    key: str
    for key in graph:
        try:
            graph[key].remove("start")
        except ValueError:
            pass

    # remove edges from the end node
    del graph["end"]

    return graph


def paths(graph: defaultdict[str, List[str]]) -> int:
    paths: List[Tuple[str, ...]] = [("start",)]
    complete: Set[Tuple[str, ...]] = set()
    while paths:
        newpaths: List[Tuple[str, ...]] = []
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


def nodupe(path: Tuple[str, ...]) -> bool:
    """return true if there is no duplicate lower-cased letter in the path"""
    ls: List[str] = [p for p in path if p.islower()]
    return len(ls) == len(set(ls))


def dubbpaths(graph: defaultdict[str, List[str]]) -> int:
    paths: List[Tuple[str, ...]] = [("start",)]
    complete: Set[Tuple[str, ...]] = set()
    while paths:
        newpaths: List[Tuple[str, ...]] = []
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

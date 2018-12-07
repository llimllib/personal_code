def do(infile):
    nodes = []
    graph = {}
    reverse = {}
    for line in infile:
        parts = line.split()
        root = parts[1]
        dep = parts[7]
        nodes.append((root, dep))
        graph.setdefault(root, []).append(dep)
        reverse.setdefault(dep, []).append(root)

    roots = list(graph.keys() - reverse.keys())
    roots.sort(reverse=True)

    root = roots.pop()
    path = [root]
    frontier = list(graph[root]) + roots
    while frontier:
        frontier.sort(reverse=True)
        nextnode = frontier.pop()
        path.append(nextnode)
        if nextnode in graph:
            for child in graph[nextnode]:
                if child not in frontier and child not in path:
                    if all(prereq in path for prereq in reverse[child]):
                        frontier.append(child)
    print("".join(path))


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))

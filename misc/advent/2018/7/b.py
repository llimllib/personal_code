def split(lst, pred):
    t, f = [], []
    for elt in lst:
        t.append(elt) if pred(elt) else f.append(elt)
    return t, f


def do(infile, nworkers, duration):
    nodes = set()
    graph = {}
    reverse = {}
    for line in infile:
        parts = line.split()
        root = parts[1]
        dep = parts[7]
        nodes.add(root)
        nodes.add(dep)
        graph.setdefault(root, []).append(dep)
        reverse.setdefault(dep, []).append(root)

    roots = list(graph.keys() - reverse.keys())
    roots.sort(reverse=True)

    workers = []
    t = 0

    try:
        for i in range(nworkers):
            root = roots.pop()
            workers.append((root, ord(root) - ord('@') + duration))
    # if there are no more roots to pop, we'll get an IndexError
    except IndexError:
        pass

    done = []
    frontier = roots
    while len(done) != len(nodes):
        # print(t, workers, done)

        # first increment the timer, and run the tasks for one more tick
        t += 1
        workers = [(node, t_remain - 1) for node, t_remain in workers]

        # remove any tasks that have completed work and add their children
        # to the frontier
        completed, workers = split(workers, lambda worker: worker[1] == 0)
        for i, (node, t_remain) in enumerate(completed):
            if t_remain == 0:
                done.append(node)
                if node in graph:
                    for child in graph[node]:
                        if child not in frontier and child not in done:
                            if all(prereq in done
                                   for prereq in reverse[child]):
                                frontier.append(child)

        frontier.sort(reverse=True)

        # if there are any open slots, fill them up with tasks from the frontier
        for i in range(nworkers - len(workers)):
            try:
                nexttask = frontier.pop()
                workers.append((nexttask, ord(nexttask) - ord('@') + duration))
            except IndexError:  # if there are no tasks to pop, just exit the loop
                break

    print(t, "".join(done))


if __name__ == "__main__":
    do(open("small.txt"), 2, 0)
    do(open("input.txt"), 5, 60)

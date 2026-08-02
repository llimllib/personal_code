from pprint import pprint
import re
import sys

import ipdb

# nodes, _ = sys.stdin.read().split("\n\n")
nodes, _ = open("sample.txt").read().split("\n\n")

dfa = {}
for node in nodes.split("\n"):
    name, *parts = re.split(r"\{|,", node)
    body = {}
    for part in parts:
        try:
            f, val = part.split(":")
            ref, op, f = re.split("(<|>)", f)
            body[val] = [ref, op, f]
        except ValueError:
            body[part.strip("}")] = None
    dfa[name] = body

pprint(dfa)


def flip(op):
    return ">=" if op == "<" else "<="


# if our target is the default node, we need to return the opposite of all
# conditions
def ifnot(dfa, node):
    conditions = []
    for cond in dfa[node]:
        if dfa[node][cond]:
            ref, op, val = dfa[node][cond]
            conditions.append([ref, flip(op), val])
    return conditions


def all_conditions(dfa, name, visited=None):
    visited = visited if visited else set()
    conditions = []
    for parent in [k for k, v in dfa.items() if name in v and k not in visited]:
        visited.add(parent)
        # if the state accepts by default, we need to add all the other
        # conditions, but in reverse
        if not dfa[parent][name]:
            conditions += ifnot(dfa, parent) + all_conditions(dfa, parent, visited)
        else:
            conditions = [dfa[parent][name]] + all_conditions(dfa, parent, visited)

    print(f"all_conditions[{name}] -> {sorted(conditions)}")
    return conditions


conditions = []
for name, node in dfa.items():
    # if we have an accepting state in the node, we want to work our way back
    # up to "in" and find all the conditions that match
    if "A" in node:
        conditions.append(
            ([node["A"]] if node["A"] else ifnot(dfa, name)) + all_conditions(dfa, name)
        )
        print(f"all conditions for {name}:")
        pprint(sorted(conditions[-1]))
pprint(sorted(conditions))

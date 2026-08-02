import re
import sys

nodes, queries = sys.stdin.read().split("\n\n")
dfa = {}
for node in nodes.split("\n"):
    name, *parts = re.split(r"\{|,", node)
    body = []
    for part in parts:
        try:
            f, val = part.split(":")
            ref, op, f = re.split("(<|>)", f)
            body.append(eval(f"lambda x: '{val}' if x['{ref}'] {op} {f} else None"))
        except ValueError:
            body.append(eval(f"lambda x: \"{part.strip('}')}\""))
    dfa[name] = body

objs = []
for q in queries.strip().split("\n"):
    objs.append(eval(q.replace("=", '":').replace("{", '{"').replace(",", ',"')))


def run_obj(dfa, o):
    cur = "in"
    while 1:
        for rule in dfa[cur]:
            new = rule(o)
            if new == "A":
                return True
            if new == "R":
                return False
            if new:
                cur = new
                break


print(sum(v for o in objs for v in o.values() if run_obj(dfa, o)))

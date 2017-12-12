from copy import copy
import itertools
import re

class Node:
    def __init__(self, name, weight, children):
        self.name = name
        self.weight = weight
        self.children = children
        self.parent = None

    def __repr__(self): return self.__str__()
    def __str__(self):
        cs = [c.name for c in self.children]
        return f"{self.weight} {self.name}: {cs} | {self.parent.name}"

index = {}

def total_weight(node):
    return node.weight + sum(total_weight(c) for c in node.children)

# XXX this is invalid in the two-children case. Not sure what to do there
def oddone(children):
    groups = [tuple(items)
            for _, items
            in itertools.groupby(children, lambda x: x[0])]
    return sorted(groups, key=lambda x: len(x))[0][0][1]

# starting from the root, follow the imbalanced nodes until we
# find one where the children all match.
def find_imbalance(node):
    children = tuple((total_weight(c), c) for c in node.children)
    if len(set(w for w, c in children)) == 1:
        return node

    return find_imbalance(oddone(children))

# given a node with the wrong weight, make it match
def fix_imbalance(node):
    weight = None
    for child in node.parent.children:
        if child != node:
            weight = total_weight(child)
            break

    # if the weight of the node is n, and the weight of all its children is m,
    # and the weight of a correct sibling is o, what we want is that n+m == o;
    # but n is wrong so n should == o-m
    return weight - sum(total_weight(c) for c in node.children)

def run():
    txt = open("input.txt")
    root = ''
    for line in txt:
        name, weight, *children = re.findall('(\w+)', line)
        node = Node(name, int(weight), children)
        index[name] = node
    allnodes = copy(index)
    while len(allnodes):
        for name, node in tuple(allnodes.items()):
            # convert the children from strings to nodes
            node.children = [index[c] for c in node.children]
            # and set their parent pointer
            for child in node.children:
                child.parent = node
            del allnodes[name]

    # found the root using vim :shrug:
    root = index["vmpywg"]
    print(fix_imbalance(find_imbalance(root)))

if __name__=="__main__":
    run()

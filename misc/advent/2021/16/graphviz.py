import json
import random
import string

# pip install pydot
import pydot

from a import Packet, PacketType, decode, to_bs

OperatorName = {
    PacketType.SUM: "+",
    PacketType.PRODUCT: "*",
    PacketType.MIN: "min",
    PacketType.MAX: "max",
    PacketType.VALUE: "val",
    PacketType.GT: ">",
    PacketType.LT: "<",
    PacketType.EQL: "=",
}

nodeids = {}


def makeid(node: Packet):
    id_ = "".join(
        random.choice(string.ascii_lowercase + string.digits) for _ in range(6)
    )
    nodeids[json.dumps(node)] = id_
    return id_


def label(node: Packet):
    if node.type == PacketType.VALUE:
        return node.value
    return OperatorName[node.type]


def addnode(graph, node: Packet):
    dot_node = pydot.Node(
        makeid(node),
        label=label(node),
        op=OperatorName[node.type],
        value=node.value,
    )
    graph.add_node(dot_node)
    for child in node.children:
        child_node = addnode(graph, child)
        edge = pydot.Edge(dot_node, child_node)
        print(edge, dot_node, child_node)
        graph.add_edge(edge)
    return dot_node


def makedot(tree, parent=None, graph=None):
    graph = pydot.Dot("day16", graph_type="digraph")
    for node in tree:
        addnode(graph, node)
    graph.write_raw("graph.dot")
    graph.write_png("out.png")


# tree, _ = decode(to_bs("9C0141080250320F1802104A08"))
# makedot(tree)

tree, _ = decode(to_bs(open("input.txt").read().strip()))
makedot(tree)

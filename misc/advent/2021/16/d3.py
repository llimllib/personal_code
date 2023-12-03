import json
from typing import List

from a import Packet, PacketType, decode, to_bs

OperatorName = {
    PacketType.SUM: "sum",
    PacketType.PRODUCT: "product",
    PacketType.MIN: "min",
    PacketType.MAX: "max",
    PacketType.VALUE: "val",
    PacketType.GT: "greater than",
    PacketType.LT: "less than",
    PacketType.EQL: "equal",
}


def json_helper(node: Packet):
    return {
        "opName": OperatorName[node.type],
        "op": node.type,
        "value": node.value,
        "children": [json_helper(n) for n in node.children],
    }


def makejson(tree: List[Packet], fout):
    assert len(tree) == 1
    return json.dump(json_helper(tree[0]), fout, indent=2)


tree, _ = decode(to_bs(open("input.txt").read().strip()))
makejson(tree, open("out.json", "w"))

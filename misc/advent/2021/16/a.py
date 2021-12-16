from typing import List, Tuple
from collections import namedtuple
from functools import reduce
from operator import mul
from enum import IntEnum

Packet = namedtuple("Packet", ["version", "type", "value", "children"])


class PacketType(IntEnum):
    SUM = 0
    PRODUCT = 1
    MIN = 2
    MAX = 3
    VALUE = 4
    GT = 5
    LT = 6
    EQL = 7


PKT_OPERATOR_T1 = "0"
PKT_OPERATOR_T2 = "1"


def decode_value(bs: str) -> Tuple[int, int]:
    vals = []
    more = True
    ptr = 6
    while more:
        more, val = True if bs[ptr] == "1" else False, bs[ptr + 1 : ptr + 5]
        vals.append(val)
        ptr += 5
    return int("".join(vals), 2), ptr - 6


def evaluate(op: int, packets: List[Packet]) -> int:
    match op:
        case PacketType.SUM:
            return sum(p.value for p in packets)
        case PacketType.PRODUCT:
            return reduce(mul, (p.value for p in packets), 1)
        case PacketType.MIN:
            return min(p.value for p in packets)
        case PacketType.MAX:
            return max(p.value for p in packets)
        case PacketType.VALUE:
            raise Exception("not sure what to do here")
        case PacketType.GT:
            assert len(packets) == 2
            return int(packets[0].value > packets[1].value)
        case PacketType.LT:
            assert len(packets) == 2
            return int(packets[0].value < packets[1].value)
        case PacketType.EQL:
            assert len(packets) == 2
            return int(packets[0].value == packets[1].value)
        case _:
            raise Exception("shouldn't get here")


def decode(bs: str, n: int = 0) -> Tuple[List[Packet], int]:
    packets = []
    bytes_read = 0
    i = 0
    while bs:
        if not (any(c == "1" for c in bs)):
            break
        if n and n == i:
            break

        version = int(bs[:3], 2)
        ptype = int(bs[3:6], 2)
        if ptype == PacketType.VALUE:
            val, nbytes = decode_value(bs)
            packets.append(Packet(version, ptype, val, []))
            bs = bs[6 + nbytes :]
            bytes_read += 6 + nbytes
        elif bs[6] == PKT_OPERATOR_T1:
            length = int(bs[7:22], 2)
            pkts, _ = decode(bs[22 : 22 + length])
            val = evaluate(ptype, pkts)
            packets.append(Packet(version, ptype, val, pkts))
            bs = bs[22 + length :]
            bytes_read += 22 + length
        elif bs[6] == PKT_OPERATOR_T2:
            length = int(bs[7:18], 2)
            pkts, nbytes = decode(bs[18:], length)
            val = evaluate(ptype, pkts)
            packets.append(Packet(version, ptype, val, pkts))
            bs = bs[18 + nbytes :]
            bytes_read += 18 + nbytes

        i += 1

    return (packets, bytes_read)


def sum_version(packets):
    version = 0
    for p in packets:
        version += p.version
        version += sum_version(p.children)
    return version


hexes = {
    "0": "0000",
    "1": "0001",
    "2": "0010",
    "3": "0011",
    "4": "0100",
    "5": "0101",
    "6": "0110",
    "7": "0111",
    "8": "1000",
    "9": "1001",
    "A": "1010",
    "B": "1011",
    "C": "1100",
    "D": "1101",
    "E": "1110",
    "F": "1111",
}


def to_bs(hexstr):
    return "".join(hexes[c] for c in hexstr)


assert to_bs("D2FE28") == "110100101111111000101000"
pkts, _ = decode(to_bs("D2FE28"))
assert len(pkts) == 1
pkt = pkts[0]
assert pkt.value == 2021
assert pkt.type == PacketType.VALUE
assert pkt.version == 6

pkts, _ = decode(to_bs("38006F45291200"))
assert len(pkts) == 1
assert pkts[0].value == 1  # 10 < 20, so this is true
assert pkts[0].children[0].value == 10
assert pkts[0].children[1].value == 20

pkts, _ = decode(to_bs("EE00D40C823060"))
assert len(pkts) == 1
assert pkts[0].value == 3  # max(1,2,3) == 3
assert pkts[0].children[0].value == 1
assert pkts[0].children[1].value == 2
assert pkts[0].children[2].value == 3

assert decode(to_bs("C200B40A82"))[0][0].value == 3
assert decode(to_bs("04005AC33890"))[0][0].value == 54
assert decode(to_bs("880086C3E88112"))[0][0].value == 7
assert decode(to_bs("CE00C43D881120"))[0][0].value == 9
assert decode(to_bs("D8005AC2A8F0"))[0][0].value == 1
assert decode(to_bs("F600BC2D8F"))[0][0].value == 0
assert decode(to_bs("9C005AC2F8F0"))[0][0].value == 0
assert decode(to_bs("9C0141080250320F1802104A08"))[0][0].value == 1

pkts, _ = decode(to_bs(open("input.txt").read().strip()))
print(sum_version(pkts))
print(pkts[0].value)

class Node():
    def __init__(self, nkids, nmeta):
        self.nkids = nkids
        self.nmeta = nmeta
        self.children = []
        self.meta = []

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"Node<{self.nkids} {self.nmeta} {self.children} {self.meta}>"


def read_kids(buf, root, nkids):
    for i in range(nkids):
        nkids, nmeta, buf = buf[0], buf[1], buf[2:]
        node = Node(nkids, nmeta)
        root.children.append(node)
        # print(f"{nkids}, {nmeta}, {n}, {buf}")
        if nkids > 0:
            buf = read_kids(buf, node, nkids)
        node.meta, buf = buf[:nmeta], buf[nmeta:]
    return buf


def val(root):
    if root.nkids == 0:
        return sum(root.meta)

    # kids are 1-indexed, so subtract one
    valid_children = [
        kid - 1 for kid in root.meta if kid > 0 and kid <= root.nkids
    ]
    return sum(val(root.children[kid]) for kid in valid_children)


def do(infile):
    buf = [int(n) for n in infile.read().strip().split()]
    root = Node(buf[0], buf[1])
    buf = read_kids(buf[2:], root, root.nkids)
    assert len(buf) == root.nmeta, f"{buf} {root.nmeta}"
    root.meta = buf
    print(val(root))
    return root


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))

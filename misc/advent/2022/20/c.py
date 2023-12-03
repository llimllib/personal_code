import sys

from ipdb import set_trace as t


class List:
    def __init__(self, ns, left=None):
        self.n = ns[0]
        self.left = left
        self.moved = False
        self.right = List(ns[1:], self) if len(ns) > 1 else None

    def print(self):
        if not self.left:
            sys.stdout.write(f"[")
        sys.stdout.write(f" {self.n} ")
        self.right.print() if self.right else sys.stdout.write(f"]")

    def length(self):
        if not self.right:
            return 1
        return 1 + self.right.length()


def find(lst, n):
    while 1:
        if lst.n == n and not lst.moved:
            return lst
        lst = lst.right


sample = [1, 2, -3, 3, -2, 0, 4]
lst = List(sample)

# make the list circular
r = lst.right
while r.right:
    r = r.right
lst.left = r
r.right = lst

for s in sample:
    node = find(lst, s)
# assert res == 3, res

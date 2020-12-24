from itertools import cycle, islice


class Cups:
    def __init__(self, ns):
        self.ns = ns
        self.cur = 0
        self.l = len(ns)

    def next3(self):
        cups = []
        for i in range(self.cur + 1, self.cur + 4):
            if i >= self.l:
                cups.append(self.ns.pop(0))
                self.cur -= 1
            else:
                cups.append(self.ns.pop(self.cur + 1))
        return cups

    def insert(self, cups, n):
        if self.cur >= n:
            self.cur += len(cups)
        self.ns = self.ns[:n] + cups + self.ns[n:]

    def getcur(self):
        return self.ns[self.cur]

    def find_dest(self):
        cur = self.getcur()
        while 1:
            cur -= 1
            if cur == 0:
                cur = max(self.ns)
            if cur in self.ns:
                return self.ns.index(cur) + 1

    def inc(self):
        self.cur = (self.cur + 1) % self.l

    def starn(self):
        idx = self.ns.index(1)
        n1, n2 = self.ns[(idx + 1) % self.l], self.ns[(idx + 2) % self.l]
        print(n1, n2)
        return n1 * n2

    def __str__(self):
        idx = self.ns.index(1)
        return "".join(map(str, self.ns[idx + 1 :] + self.ns[0:idx]))

    def __repr__(self):
        parts = []
        for i, cup in enumerate(self.ns):
            if i == self.cur:
                parts.append(f"({cup})")
            else:
                parts.append(f"{cup}")
        return "cups " + " ".join(parts)


def pt1(inp: str, rounds: int = 100, verbose=False):
    ns = list(int(n) for n in inp)
    c = Cups(ns)

    for i in range(rounds):
        if verbose:
            print(repr(c))
        n3 = c.next3()
        dest = c.find_dest()
        c.insert(n3, dest)
        c.inc()

    return str(c)


def pt2(inp: str, rounds: int = 10_000_000):
    ns = list(int(n) for n in inp) + list(range(10, 1000001))
    assert len(ns) == 1000000, len(ns)
    c = Cups(ns)

    for i in range(rounds):
        if i % 100 == 0:
            print(i)
        n3 = c.next3()
        dest = c.find_dest()
        c.insert(n3, dest)
        c.inc()

    return ""


n = pt1("389125467", rounds=10, verbose=True)
assert n == "92658374", f"{n} != 92658374"
n = pt1("389125467")
assert n == "67384529", f"{n} != 67384529"

print(f"part 1: {pt1('653427918')}")

n = pt2("389125467", 100)
assert n == 149245887792, f"{n} != 149245887792"

# too slow to complete before the heat death of the universe
# print(f"part 2: {pt2('653427918')}")

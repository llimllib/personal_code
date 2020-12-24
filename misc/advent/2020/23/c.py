def p(rnd, ns, idx):
    parts = []
    for i, n in enumerate(ns):
        if i == idx:
            parts.append(f"({n})")
        else:
            parts.append(str(n))
    print(f"{rnd}: {' '.join(parts)}")

def loop(ns, rounds, verbose=False, pt1=False):
    cur = 0
    l = len(ns)
    m = max(ns)
    n3 = [-1, -1, -1]

    for i in range(rounds):
        if verbose:
            p(i + 1, ns, cur)
        if not (i+1) % 10000:
            print(i+1)
        n3[0] = ns[(cur + 1) % l]
        n3[1] = ns[(cur + 2) % l]
        n3[2] = ns[(cur + 3) % l]
        ns[(cur + 1) % l] = -1
        ns[(cur + 2) % l] = -1
        ns[(cur + 3) % l] = -1
        destcup = ns[cur] - 1 if ns[cur] > 1 else m
        while destcup in n3:
            destcup = destcup - 1 if destcup > 1 else m
        try:
            destidx = ns.index(destcup, start=cur)
        except ValueError:
            destidx = ns.index(destcup)
        if cur > destidx:
            for j in range(cur, destidx, -1):
                ns[(j + 3) % l] = ns[j]
            cur = (cur + 3) % l
        else:
            for j in range(l + cur, destidx, -1):
                if j >= l:
                    ns[(j % l) + 3] = ns[j % l]
                else:
                    ns[(j + 3) % l] = ns[j]
            cur = (cur + 3) % l
        ns[(destidx + 1) % l] = n3[0]
        ns[(destidx + 2) % l] = n3[1]
        ns[(destidx + 3) % l] = n3[2]
        cur = (cur + 1) % l

    one = ns.index(1)
    if pt1:
        return "".join(map(str, ns[one + 1 :] + ns[:one]))
    else:
        return ns[(one+1)%l] * ns[(one+2)%l]


def pt1(inp: str, rounds: int = 100, verbose=False):
    ns = list(int(n) for n in inp)
    return loop(ns, rounds, verbose, True)


def pt2(inp: str, rounds: int = 10000000):
    ns = list(int(n) for n in inp) + list(range(10, 1000001))
    assert len(ns) == 1000000, len(ns)
    return loop(ns, rounds)

n = pt1("389125467", rounds=10, verbose=True)
assert n == "92658374", f"{n} != 92658374"
# n = pt1("389125467")
# assert n == "67384529", f"{n} != 67384529"

# print(f"part 1: {pt1('653427918')}")

# n = pt2("389125467", rounds=100000)
# assert n == 149245887792, f"{n} != 149245887792"

print(f"part 2: {pt2('653427918')}")

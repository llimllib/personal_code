class Planet:
    def __init__(self, name, parent):
        self.name = name
        self.parent = parent
        self.children = set()

    def __str__(self):
        return f"{self.parent.name if self.parent else 'root'} -> *{self.name}* -> {[c.name for c in self.children]}"

    def __repr__(self):
        return self.__str__()


COM = Planet("COM", None)
index = {"COM": COM}

# first we'll just make an object for each planet
for line in open("in"):
    planeta, planetb = line.strip().split(")")
    if planeta in index:
        planeta = index[planeta]
    else:
        planeta = Planet(planeta, None)
        index[planeta.name] = planeta

    if planetb in index:
        planetb = index[planetb]
        planetb.parent = planeta
    else:
        planetb = Planet(planetb, planeta)
        index[planetb.name] = planetb

    planeta.children.add(planetb)

you = index["YOU"]
santa = index["SAN"]

# first compute the lowest common ancestor; this is just the first intersection
# in the two paths to the root
yfrontier = set([you.parent])
sfrontier = set([santa.parent])
while 1:
    if yfrontier & sfrontier:
        print(yfrontier & sfrontier)
        lca = (yfrontier & sfrontier).pop()
        break
    for node in yfrontier.copy():
        if node.parent:
            yfrontier.add(node.parent)
    for node in sfrontier.copy():
        if node.parent:
            sfrontier.add(node.parent)


def plen(pfrom, pto):
    i = 0
    while pfrom.parent != pto:
        i += 1
        pfrom = pfrom.parent
    return i


# now the shortest path is the sum of the paths to the lca
print(plen(you, lca) + plen(santa, lca))

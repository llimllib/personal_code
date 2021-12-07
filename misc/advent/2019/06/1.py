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


def tlen(planet):
    i = 0
    while planet.parent:
        i += 1
        planet = planet.parent
    return i


print(sum(tlen(planet) for planet in index.values()))

import sys

LOW = 0
HIGH = 1
DEBUG = False


class Broadcaster:
    def __init__(self, targets):
        self.targets = targets

    def __call__(self, _, __, queue):
        for t in self.targets:
            queue.append(("broadcaster", t, LOW))


class FlippyFloppy:
    OFF = 0
    ON = 1

    def __init__(self, targets, name):
        self.state = FlippyFloppy.OFF
        self.targets = targets
        self.name = name

    def __call__(self, _, pulse, queue):
        if DEBUG:
            print(f"{sender} -{pulse}-> {self.name}")

        if pulse == HIGH:
            return

        if self.state == FlippyFloppy.OFF:
            self.state = FlippyFloppy.ON
            signal = HIGH
        else:
            self.state = FlippyFloppy.OFF
            signal = LOW

        for t in self.targets:
            queue.append((self.name, t, signal))


class Conjunction:
    def __init__(self, targets, name):
        self.state = {}
        self.targets = targets
        self.name = name

    def __call__(self, sender, pulse, queue):
        if DEBUG:
            print(f"{sender} -{pulse}-> {self.name}")

        self.state[sender] = pulse

        if all(v == HIGH for v in self.state.values()):
            signal = LOW
        else:
            signal = HIGH

        for t in self.targets:
            queue.append((self.name, t, signal))


items = {}
for line in sys.stdin:
    item, targets = line.strip().split("->")
    item, name = item[0], item[1:].strip()
    targets = [t.strip() for t in targets.split(",")]
    match item:
        case "b":
            items[name] = Broadcaster(targets)
        case "%":
            items[name] = FlippyFloppy(targets, name)
        case "&":
            items[name] = Conjunction(targets, name)

# To turn the dot file into an svg:
#   dot -Tsvg g.dot > graph.svg
with open("g.dot", "w") as fout:
    fout.write("digraph G {\n")
    for name, item in items.items():
        shape = "rectangle" if type(item) == FlippyFloppy else "circle"
        fout.write(f"  {name} [shape={shape}]\n")
    fout.write(f"  rx [shape=invtriangle style=filled fillcolor=red]\n")
    fout.write("\n")
    for name, item in items.items():
        for t in item.targets:
            fout.write(f"  {name} -> {t}\n")
    fout.write("}\n")

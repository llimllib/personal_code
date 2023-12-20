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

# set the default state to low for all inputs of conjunctions
for name, item in items.items():
    for target in item.targets:
        if target in items and type(items[target]) == Conjunction:
            items[target].state[name] = LOW

counts = {0: 0, 1: 0}
found = False
i = 0
for i in range(1000):
    queue = []
    counts[LOW] += 1  # button press
    items["roadcaster"](None, None, queue)
    while queue:
        sender, target, signal = queue.pop(0)
        counts[signal] += 1
        if target not in items:
            if DEBUG:
                print(f"ignoring {sender} -{signal}-> {target}")
            continue
        items[target](sender, signal, queue)

print("part 1:", counts[LOW] * counts[HIGH])
